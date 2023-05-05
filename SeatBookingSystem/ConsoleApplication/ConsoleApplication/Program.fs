// Define a record type Ticket with two fields: seat (an integer) and customer (a string)
type Ticket = { seat:int; customer:string }

// Initialize a mutable list of tickets with seat numbers from 1 to 10 and no customer names
let mutable tickets = [ for n in 1..10 -> { seat = n; customer = "" } ]

// Define a function to display the list of tickets in a formatted table
let DisplayTickets () =
    printfn "%-5s %-15s" "Seat Number" "Customer Name"
    printfn "%-5s %-15s" "___________---_____________"
    for ticket in tickets do
        printfn "    %-5d      %-15s" ticket.seat ticket.customer

// Created the object lockObj used as a lock and can be taken off after update 
let lockObj = obj()

// Define a function to book a seat with a given customer name and seat number
let BookSeat (customerName:string, seatNumber:int) =
    // Define a local function to assign a customer name to a ticket with a given seat number
    let assignCustomer (ticket:Ticket) =
        if ticket.seat = seatNumber then
            { ticket with customer = customerName }
        else
            ticket
    System.Threading.Monitor.Enter(lockObj) // Enter the critical section
    try
    // Map over the list of tickets and update the ticket with the given seat number
    tickets <- List.map assignCustomer tickets
    finally
    System.Threading.Monitor.Exit(lockObj) // Exit the critical section
    
// Define a function to prompt the user for input and provide a simple command line interface
let menu () =
    while true do
        printfn "\nPlease select an option:"
        printfn "1. Display tickets"
        printfn "2. Book a seat"
        printfn "3. Exit"
        let input = System.Console.ReadLine()
        match input with
        | "1" -> DisplayTickets ()
        | "2" -> printfn "Please enter customer name:"
                 let customerName = System.Console.ReadLine()
                 printfn "Please enter seat number:"
                 let seatNumber = int (System.Console.ReadLine())
                 BookSeat (customerName, seatNumber)
        | "3" -> exit 0
        | _ -> printfn "Invalid input. Please try again."

// Call the menu function to start the ticket booking program
menu ()