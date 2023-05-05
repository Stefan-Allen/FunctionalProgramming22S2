// Define an an account types: Checking and Savings.
type AccountType =
    | Checking
    | Savings

// Define a class for an Account with accountNumber, accountType, and initialBalance fields.
type Account(accountNumber: string, accountType: AccountType, initialBalance: float32) =
    // Define a mutable balance field with the initial balance value.
    let mutable balance: float32 = initialBalance

    // Read-only properties for accountNumber, accountType, and balance.
    member this.AccountNumber = accountNumber
    member this.AccountType = accountType
    member this.Balance = balance

    // Define methods for withdrawing and depositing money from the account.
    member this.Withdrawal(amount: float32) =
        if amount > balance then
            printfn "Not Enough Funds, Transaction Cancelled!"
            None
        else
            balance <- balance - amount
            Some amount
// Define methods for depositing and depositing money from the account.
    member this.Deposit(amount: float32) =
        balance <- balance + amount
        Some amount

    // method for printing the account details to the console.
    member this.Print() =
        printfn "Account Type: %A, Account Number: %s, Balance: %.2f" this.AccountType this.AccountNumber balance
// function to check account balance status and print a message to the console.
let CheckAccount (account: Account) =
    match account.AccountType with
    | Checking ->
        match account.Balance with
        | balance when balance < 10.0f -> printfn "Account Balance IS LOW"
        | balance when balance >= 10.0f && balance <= 100.0f -> printfn "Account Balance IS OK"
        | _ -> printfn "Account Balance IS HIGH"
    | Savings ->
        match account.Balance with
        | balance when balance < 50.0f -> printfn "Account Balance IS LOW"
        | balance when balance >= 50.0f && balance <= 500.0f -> printfn "Account Balance IS OK"
        | _ -> printfn "Account Balance IS HIGH"


// list of accounts 
let accounts =
    [ Account("0001", Savings, 0.0f)
      Account("0003", Checking, 5.0f)
      Account("988679", Checking, 0.0f)
      Account("363152", Savings, 49.0f)
      Account("302094", Checking, 25.0f)
      Account("0002", Checking, 50.0f)
      Account("375470", Savings, 100.0f)
      Account("170857", Savings, 65.0f)
      Account("264587", Savings, 54.0f)
      Account("607345", Checking, 150.0f) ]

   
// recursive function to execute various actions based on user input.
let rec executeFunction (accounts: Account list) : unit =
    
    // set keepRunning to true meaning menu can run even when exits to 0 
    let mutable keepRunning = true

    // when Keep running is enabled system to rerun it self 
    while keepRunning do
    printfn "Selector: "
    printfn "1. Display Accounts: "
    printfn "2. Print Account By ID: "
    printfn "3. Deposit Money To Bank: "
    printfn "4. Withdraw Money: "
    printfn "5. Print Accounts Less Than $50 "
    printfn "6. Print Accounts $50 And Over"
    printfn "7. Check Account Balance Status"
    printfn "8. Exit"

    // Read the user's choice from the console.
    let choice = System.Console.ReadLine()

    // Execute Appropriate action based on the user's choice.
    match choice with
    | "1" ->
        accounts |> List.iter (fun acc -> acc.Print())
        executeFunction accounts
    | "2" ->
        // Prompt the user for an account number and print the account details.
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
        // Prompt the user for an account number and deposit amount, and update the account balance.
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
        // Prompt the user for an account number and withdrawal amount, and update the account balance.
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
        // sequence loop for checking if accounts are under 50
        printfn "Accounts under 50£"
        let lessThan50Seq = accounts |> Seq.filter (fun acc -> acc.Balance < 50.0f)
        for account in lessThan50Seq do
            account.Print()
    | "6" ->
        // sequence loop for checking if accounts are above 50
        printfn "Accounts above 50£"
        let greaterThan50Seq = accounts |> Seq.filter (fun acc -> acc.Balance >= 50.0f)
        for account in greaterThan50Seq do
            account.Print()
    | "7" ->
        // Check and print the balance status for each account.
        accounts |> List.iter (fun acc -> CheckAccount acc)
        executeFunction accounts
    | "8" ->
        // Exit the program.
        printfn "Input Not Recognized try again!"
        executeFunction accounts
    | _ ->
        // Prompt the user for a valid input.
        printfn "Input Not Recognized try again!"
        executeFunction accounts
// Executes the list function
executeFunction(accounts)


