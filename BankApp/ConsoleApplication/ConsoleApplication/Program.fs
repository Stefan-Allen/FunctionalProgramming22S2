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
            printfn "Not Enough Funds, Transaction Cancelled!"
        else
            balance <- balance - amount

    member this.Deposit(amount: float32) = balance <- balance + amount

    member this.Print() =
        printfn "Account Type: %A, Account Number: %s, Balance: %.2f" this.AccountType this.AccountNumber balance

let CheckAccount (account: Account) =
    match account.AccountType with
    | Checking ->
        match account.Balance with
        | balance when balance < 10.0f -> printfn "Account Balance IS LOW"
        | balance when balance >= 10.0f && balance <= 100.0f -> printfn "Account Balance IS OK"
        | _ -> printfn "Account Balance IS HIGH"
    | Savings ->
        match account.Balance with
        | balance when balance < 50.0f -> printfn "Account Balance IS lOW"
        | balance when balance >= 50.0f && balance <= 500.0f -> printfn "Account Balance IS OK"
        | _ -> printfn "Account Balance IS HIGH"

let accountsless50 =
    [ Account("0001", Savings, 0.0f)
      Account("0003", Checking, 5.0f)
      Account("988679", Checking, 0.0f)
      Account("363152", Savings, 49.0f)
      Account("302094", Checking, 25.0f) ]

let accounts50plus =
    [ Account("0002", Checking, 50.0f)
      Account("375470", Savings, 100.0f)
      Account("170857", Savings, 65.0f)
      Account("264587", Savings, 54.0f)
      Account("607345", Checking, 150.0f) ]

let rec executeFunction (accounts: Account list) =
    printfn "Selector: "
    printfn "1. Display Accounts: "
    printfn "2. Print Account By ID: "
    printfn "3. Deposit Money To Bank: "
    printfn "4. Withdraw Money: "
    printfn "5. Print Accounts Less Than $50 "
    printfn "6. Print Accounts $50 And Over"
    printfn "7. Check Account Balance Status"
    printfn "8. Exit"

    let choice = System.Console.ReadLine()

    match choice with
    | "1" ->
        accounts |> List.iter (fun acc -> acc.Print())
        executeFunction accounts
    | "2" ->
        printfn "Enter Account ID: "
        let accountNumber = System.Console.ReadLine()

        match accounts |> List.tryFind (fun acc -> acc.AccountNumber = accountNumber) with
        | Some account ->
            account.Print()
            executeFunction accounts
        | None ->
            printfn "Account Not Found In System."
            executeFunction accounts
    | "3" ->
        printfn "Enter Account ID: "
        let accountNumber = System.Console.ReadLine()

        match accounts |> List.tryFind (fun acc -> acc.AccountNumber = accountNumber) with
        | Some account ->
            printfn "How Much Would You Like To Add To Bank Account? "
            let depositAmount = System.Console.ReadLine() |> float32
            account.Deposit(depositAmount)
            executeFunction accounts
        | None ->
            printfn "Account Not Found In System."
            executeFunction accounts
    | "4" ->
        printfn "Enter Account ID: "
        let accountNumber = System.Console.ReadLine()

        match accounts |> List.tryFind (fun acc -> acc.AccountNumber = accountNumber) with
        | Some account ->
            printfn "How Much Would You Like To Takeout To Bank Account? "
            let withdrawalAmount = System.Console.ReadLine() |> float32
            account.Withdrawal(withdrawalAmount)
            executeFunction accounts
        | None ->
            printfn "Account Not Found In System."
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
        printfn "Input Not Recognized try again!"
        executeFunction accounts

let printAccounts (accounts: Account list) =
    accounts |> List.iter (fun acc -> acc.Print())

printAccounts (accountsless50 @ accounts50plus)
executeFunction (accountsless50 @ accounts50plus)
