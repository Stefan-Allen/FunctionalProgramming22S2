type AccountType =
    | Checking
    | Savings

type Account(accountNumber: string, accountType: AccountType, initialBalance: float32) =
    let mutable balance = initialBalance

    member this.AccountNumber = accountNumber
    member this.AccountType = accountType
    member this.Balance = balance

    member this.Withdrawal(amount: float32) =
        if amount > balance then
            printfn "Transaction cancelled: Insufficient balance"
        else
            balance <- balance - amount

    member this.Deposit(amount: float32) =
        balance <- balance + amount

    member this.Print() =
        printfn "Account Number: %s, Account Type: %A, Balance: %.2f" this.AccountNumber this.AccountType balance

let CheckAccount (account: Account) =
    match account.AccountType with
    | Checking ->
        match account.Balance with
        | balance when balance < 10.0f -> printfn "Balance is low"
        | balance when balance >= 10.0f && balance <= 100.0f -> printfn "Balance is OK"
        | _ -> printfn "Balance is high"
    | Savings ->
        match account.Balance with
        | balance when balance < 50.0f -> printfn "Balance is low"
        | balance when balance >= 50.0f && balance <= 500.0f -> printfn "Balance is OK"
        | _ -> printfn "Balance is high"

let accountsless50 =
    [ Account("0001", Checking, 0.0f);
      Account("0003", Savings, 5.0f);
      Account("988679", Checking, 0.0f);
      Account("363152", Savings, 49.0f);
      Account("302094", Savings, 25.0f) ]
    
let accounts50plus =
    [ Account("0002", Checking, 50.0f);
      Account("375470", Checking, 100.0f);
      Account("170857", Checking, 65.0f);
      Account("264587", Checking, 54.0f);
      Account("607345", Checking, 150.0f) ]

let rec executeFunction (accounts: Account list) =
    printfn "Please select an option:"
    printfn "1. Print all accounts"
    printfn "2. Print account by account number"
    printfn "3. Deposit"
    printfn "4. Withdraw"
    printfn "5. Print accounts less than $50 "
    printfn "6. Print accounts $50 and over"
    printfn "7. Check account balance status"
    printfn "8. Exit"

    let choice = System.Console.ReadLine()

    match choice with
    | "1" ->
        accounts |> List.iter (fun acc -> acc.Print())
        executeFunction accounts
    | "2" ->
        printfn "Please enter the account number: "
        let accountNumber = System.Console.ReadLine()

        match accounts |> List.tryFind (fun acc -> acc.AccountNumber = accountNumber) with
        | Some account ->
            account.Print()
            executeFunction accounts
        | None ->
            printfn "Account not found."
            executeFunction accounts
    | "3" ->
        printfn "Please enter the account number: "
        let accountNumber = System.Console.ReadLine()

        match accounts |> List.tryFind (fun acc -> acc.AccountNumber = accountNumber) with
        | Some account ->
            printfn "How much would you like to deposit?"
            let depositAmount = System.Console.ReadLine() |> float32
            account.Deposit(depositAmount)
            executeFunction accounts
        | None ->
            printfn "Account not found."
            executeFunction accounts
    | "4" ->
        printfn "Please enter the account number: "
        let accountNumber = System.Console.ReadLine()

        match accounts |> List.tryFind (fun acc -> acc.AccountNumber = accountNumber) with
        | Some account ->
            printfn "How much would you like to withdraw?"
            let withdrawalAmount = System.Console.ReadLine() |> float32
            account.Withdrawal(withdrawalAmount)
            executeFunction accounts
        | None ->
            printfn "Account not found."
            executeFunction accounts
    | "5" ->
        accountsless50 |> List.iter (fun acc -> acc.Print())
        executeFunction accounts
    | "6" ->
        accounts50plus |> List.iter (fun acc -> acc.Print())
        executeFunction accounts
    | "7" ->
        accounts |> List.iter (fun acc -> CheckAccount acc)
        executeFunction accounts
    | "8" -> ()
    | _ ->
        printfn "Invalid input, please try again."
        executeFunction accounts
        
let printAccounts (accounts: Account list) =
    accounts |> List.iter (fun acc -> acc.Print())

printAccounts (accountsless50 @ accounts50plus)
executeFunction (accountsless50 @ accounts50plus)

