// --- Teljes, Dexscreener-only F# Telegram bot ---

open System
open System.IO
open System.Globalization
open System.Net.Http
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open Telegram.Bot
open Telegram.Bot.Polling
open Telegram.Bot.Types
open Telegram.Bot.Types.Enums
open FSharp.Control.Tasks.V2.ContextInsensitive
open Telegram.Bot.Types.ReplyMarkups

// --- Biztonságos Task wrapper ---
let safeTask (f: unit -> Task<'T>) (onError: exn -> 'T) = task {
    try return! f() with ex -> return onError ex
}

// --- Konfiguráció ---
let token = Environment.GetEnvironmentVariable "TELEGRAM_TOKEN"
let openAiKey = Environment.GetEnvironmentVariable "OPENAI_API_KEY"
if String.IsNullOrWhiteSpace token then failwith "Missing TELEGRAM_TOKEN!"
if String.IsNullOrWhiteSpace openAiKey then failwith "Missing OPENAI_API_KEY!"

let botClient = TelegramBotClient(token)
let cts = new CancellationTokenSource()
let receiverOptions = ReceiverOptions(AllowedUpdates = [||])

// --- Állapot mentése ---
type AlphaCall = { Name: string; CA: string; ChainId: string; StartPrice: string }
let alphaCalls = ResizeArray<AlphaCall>()

let stateFile = "state.json"
let saveState () =
    try
        let json = JsonSerializer.Serialize(alphaCalls, JsonSerializerOptions(WriteIndented = true))
        File.WriteAllText(stateFile, json)
    with _ -> ()

let loadState () =
    try
        if File.Exists(stateFile) then
            let json = File.ReadAllText(stateFile)
            let calls = JsonSerializer.Deserialize<AlphaCall list>(json)
            alphaCalls.Clear()
            for c in calls do alphaCalls.Add(c)
    with _ -> ()

// --- OpenAI extraction típus ---
type AlphaExtractionResult = { CA: string; ChainId: string }

let extractAlphaData (text: string) =
    safeTask
        (fun () ->
            task {
                use client = new HttpClient()
                client.DefaultRequestHeaders.Authorization <- System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", openAiKey)
                let prompt =
                    $"""From the following text, extract:
- The blockchain contract address (CA).
- The Dexscreener chainId (ONLY text values like: ethereum, bsc, polygon, avalanche, fantom, arbitrum, optimism, base, etc. DO NOT return any number like '1' or '56').

Return exactly in this format:
CA: <contract address>
CHAINID: <chain id text name>

Text:
{text}"""
                let payload = {| model = "gpt-4.1-nano"; messages = [| {| role = "user"; content = prompt |} |] |}
                let! resp = client.PostAsync(
                    "https://api.openai.com/v1/chat/completions",
                    new StringContent(JsonSerializer.Serialize payload, Encoding.UTF8, "application/json"),
                    cancellationToken = cts.Token)
                resp.EnsureSuccessStatusCode() |> ignore
                let! body = resp.Content.ReadAsStringAsync()
                let doc = JsonDocument.Parse(body)
                let content = doc.RootElement.GetProperty("choices").[0].GetProperty("message").GetProperty("content").GetString()
                let lines = content.Split('\n', StringSplitOptions.RemoveEmptyEntries)
                let ca =
                    lines
                    |> Array.tryPick (fun l ->
                        if l.StartsWith("CA:", StringComparison.OrdinalIgnoreCase) then Some(l.Substring(3).Trim()) else None)
                    |> Option.defaultValue "NONE"
                let chain =
                    lines
                    |> Array.tryPick (fun l ->
                        if l.StartsWith("CHAINID:", StringComparison.OrdinalIgnoreCase) then Some(l.Substring(8).Trim().ToLowerInvariant()) else None)
                    |> Option.defaultValue "UNKNOWN"
                let finalChain = if Regex.IsMatch(chain, "^\\d+$") then "UNKNOWN" else chain
                return { CA = ca; ChainId = finalChain }
            })
        (fun _ -> { CA = "NONE"; ChainId = "UNKNOWN" })

// --- Dexscreener lekérdező ---
type TokenInfo = { Name: string; PriceUsd: string }

let getDexscreenerTokenInfo (ca: string) (chainId: string) =
    safeTask (fun () -> task {
        use client = new HttpClient()
        let url = sprintf "https://api.dexscreener.com/latest/dex/tokens/%s" ca
        let! resp = client.GetAsync(url, cancellationToken = cts.Token)
        if resp.IsSuccessStatusCode then
            let! body = resp.Content.ReadAsStringAsync()
            let doc = JsonDocument.Parse(body)
            let pairs = doc.RootElement.GetProperty("pairs").EnumerateArray()
            let poolOpt = pairs |> Seq.tryFind (fun p -> p.GetProperty("chainId").GetString().Equals(chainId, StringComparison.OrdinalIgnoreCase))
            match poolOpt with
            | Some pool ->
                let baseToken = pool.GetProperty("baseToken")
                let name = baseToken.GetProperty("name").GetString()
                let priceProp = pool.GetProperty("priceUsd")
                let price =
                    match priceProp.ValueKind with
                    | JsonValueKind.Number -> priceProp.GetDecimal().ToString(CultureInfo.InvariantCulture)
                    | JsonValueKind.String -> priceProp.GetString()
                    | _ -> "N/A"
                return Ok { Name = name; PriceUsd = price }
            | None -> return Error "No matching pool for given chainId"
        else return Error resp.ReasonPhrase
    }) (fun ex -> Error ex.Message)

// --- Init és parancskezelők ---
let initAlpha (ca: string) (chainId: string) =
    safeTask (fun () -> task {
        let! infoRes = getDexscreenerTokenInfo ca chainId
        match infoRes with
        | Ok info ->
            let call = { Name = info.Name; CA = ca; ChainId = chainId; StartPrice = info.PriceUsd }
            alphaCalls.Add(call)
            saveState()
            return Ok call
        | Error e -> return Error e
    }) (fun ex -> Error ex.Message)

let sendMessageAsync (bot: ITelegramBotClient) (chatId: int64) (text: string) (ct: CancellationToken) =
    task {
        try
            do bot.SendMessage(ChatId.op_Implicit(chatId), text, cancellationToken = ct) |> ignore
        with ex ->
            printfn "[SEND ERROR] %s" ex.Message
    }

let handleAddAlpha (bot: ITelegramBotClient) (msg: Message) (args: string) (ct: CancellationToken) = task {
    if String.IsNullOrWhiteSpace args then
        do! sendMessageAsync bot msg.Chat.Id "Usage: /addalphacall <description>" ct
    else
        do! sendMessageAsync bot msg.Chat.Id "Processing…" ct
        let! ext = extractAlphaData args
        if ext.CA = "NONE" || ext.ChainId = "UNKNOWN" then
            do! sendMessageAsync bot msg.Chat.Id "❌ Extraction failed." ct
        else
            let! result = initAlpha ext.CA ext.ChainId
            match result with
            | Ok c -> do! sendMessageAsync bot msg.Chat.Id (sprintf "✅ Saved: %s @ %s USD" c.Name c.StartPrice) ct
            | Error e ->
                printfn "[BOT ERROR] %s" e
    return ()
}

let handleGetAlphaByChatId (bot: ITelegramBotClient) (chatId: int64) (ct: CancellationToken) = task {
    if alphaCalls.Count = 0 then
        do! sendMessageAsync bot chatId "No alpha calls." ct
    else
        let sb = StringBuilder()
        for i in 0 .. alphaCalls.Count - 1 do
            let c = alphaCalls.[i]
            let! infoRes = getDexscreenerTokenInfo c.CA c.ChainId
            match infoRes with
            | Ok info ->
                let now = Double.Parse(info.PriceUsd, NumberStyles.Float, CultureInfo.InvariantCulture)
                let start = Double.Parse(c.StartPrice, NumberStyles.Float, CultureInfo.InvariantCulture)
                let change = if start > 0. then (now - start) / start * 100. else 0.
                sb.Append(sprintf "🔸 Alpha %d:\n • Name: %s\n • CA: %s\n • Chain: %s\n • Start: %s USD\n • Now: %s USD\n • Δ: %.2f%%\n\n" (i+1) c.Name c.CA c.ChainId c.StartPrice info.PriceUsd change) |> ignore
            | Error e ->
                printfn "[ALPHA ERROR] %s" e
        do! sendMessageAsync bot chatId (sb.ToString()) ct
    return ()
}

let handleRemoveAlpha (bot: ITelegramBotClient) (msg: Message) (args: string) (ct: CancellationToken) = task {
    let parseResult = System.Int32.TryParse(args)
    if parseResult |> fst && (let idx = snd parseResult in idx > 0 && idx <= alphaCalls.Count) then
        let idx = snd parseResult
        let removed = alphaCalls.[idx - 1]
        alphaCalls.RemoveAt(idx - 1)
        saveState()
        do! sendMessageAsync bot msg.Chat.Id (sprintf "✅ Removed alpha call #%d: %s" idx removed.Name) ct
    else
        do! sendMessageAsync bot msg.Chat.Id "Please provide a valid index. Usage: /removecall <index>" ct
    return ()
}

let handleChangeStart (bot: ITelegramBotClient) (msg: Message) (args: string) (ct: CancellationToken) = task {
    let parts = args.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    if parts.Length <> 2 then
        do! sendMessageAsync bot msg.Chat.Id "Usage: /changestart <index> <newprice>" ct
    else
        let idxParsed, idx = System.Int32.TryParse(parts.[0])
        let priceParsed, newPrice = Double.TryParse(parts.[1], System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture)
        if not idxParsed || not priceParsed || idx <= 0 || idx > alphaCalls.Count || newPrice < 0.0 then
            do! sendMessageAsync bot msg.Chat.Id "Invalid index or price. Usage: /changestart <index> <newprice>" ct
        else
            let call = alphaCalls.[idx - 1]
            alphaCalls.[idx - 1] <- { call with StartPrice = newPrice.ToString(System.Globalization.CultureInfo.InvariantCulture) }
            saveState()
            do! sendMessageAsync bot msg.Chat.Id (sprintf "✅ Start price for #%d (%s) changed to %s USD" idx call.Name (newPrice.ToString(System.Globalization.CultureInfo.InvariantCulture))) ct
    return ()
}

let mutable loopActive = false
let mutable loopInterval = 5 // alapértelmezett: 5 perc
let mutable loopCts = new CancellationTokenSource()

let rec alphaLoop (bot: ITelegramBotClient) (chatId: int64) = task {
    while loopActive do
        try
            do! handleGetAlphaByChatId bot chatId cts.Token
        with ex ->
            printfn "[LOOP ERROR] %s" ex.Message
        do! Task.Delay(TimeSpan.FromMinutes(float loopInterval), loopCts.Token)
}

let handleStart (bot: ITelegramBotClient) (msg: Message) (ct: CancellationToken) = task {
    if not loopActive then
        loopActive <- true
        loopCts.Dispose()
        let newCts = new CancellationTokenSource()
        loopCts <- newCts // FIX: assign directly instead of using Interlocked.Exchange
        do! sendMessageAsync bot msg.Chat.Id "Alpha loop started." ct
        alphaLoop bot msg.Chat.Id |> ignore
    else
        do! sendMessageAsync bot msg.Chat.Id "Loop already running." ct
}

let handleStop (bot: ITelegramBotClient) (msg: Message) (ct: CancellationToken) = task {
    if loopActive then
        loopActive <- false
        loopCts.Cancel()
        do! sendMessageAsync bot msg.Chat.Id "Alpha loop stopped." ct
    else
        do! sendMessageAsync bot msg.Chat.Id "Loop is not running." ct
}

let handleInterval (bot: ITelegramBotClient) (msg: Message) (args: string) (ct: CancellationToken) = task {
    match System.Int32.TryParse(args) with
    | true, v when v > 0 ->
        loopInterval <- v
        do! sendMessageAsync bot msg.Chat.Id (sprintf "Interval set to %d minutes." v) ct
    | _ ->
        do! sendMessageAsync bot msg.Chat.Id "Usage: /interval <minutes> (must be positive integer)" ct
}

let handleUpdate (bot: ITelegramBotClient) (update: Update) (ct: CancellationToken) = task {
    match update.Message with
    | null -> ()
    | msg when isNull msg.Text -> ()
    | msg ->
        // --- ADMIN CHECK minden parancsra ---
        if isNull msg.From then
            do! sendMessageAsync bot msg.Chat.Id "❌ Only admins." ct
        else
            let! chatMember = bot.GetChatMember(msg.Chat.Id, msg.From.Id)
            let isAdmin = chatMember.Status = ChatMemberStatus.Administrator || chatMember.Status = ChatMemberStatus.Creator
            if not isAdmin then
                do! sendMessageAsync bot msg.Chat.Id "❌ Only admins." ct
            else
                if msg.Text.StartsWith("/addalphacall") then
                    let args = msg.Text.Substring("/addalphacall".Length).Trim()
                    do! handleAddAlpha bot msg args ct
                elif msg.Text.StartsWith("/getalpha") then
                    do! handleGetAlphaByChatId bot msg.Chat.Id ct
                elif msg.Text.StartsWith("/removecall") then
                    let args = msg.Text.Substring("/removecall".Length).Trim()
                    do! handleRemoveAlpha bot msg args ct
                elif msg.Text.StartsWith("/start") then
                    do! handleStart bot msg ct
                elif msg.Text.StartsWith("/stop") then
                    do! handleStop bot msg ct
                elif msg.Text.StartsWith("/interval") then
                    let args = msg.Text.Substring("/interval".Length).Trim()
                    do! handleInterval bot msg args ct
                elif msg.Text.StartsWith("/changestart") then
                    let args = msg.Text.Substring("/changestart".Length).Trim()
                    do! handleChangeStart bot msg args ct
                else ()
}

let handleError (bot: ITelegramBotClient) (ex: Exception) (ct: CancellationToken) = task {
    printfn "Error: %O" ex
}

[<EntryPoint>]
let main _ =
    loadState()
    let updateHandler (bot: ITelegramBotClient) (update: Update) (ct: CancellationToken) =
        handleUpdate bot update ct |> Async.AwaitTask |> Async.RunSynchronously
    let errorHandler (bot: ITelegramBotClient) (ex: exn) (ct: CancellationToken) =
        handleError bot ex ct |> Async.AwaitTask |> Async.RunSynchronously
    botClient.StartReceiving(updateHandler, errorHandler, receiverOptions, cts.Token)
    printfn "Bot started. Use /addalphacall and /getalpha."
    Task.Delay(-1, cts.Token).GetAwaiter().GetResult()
    0
