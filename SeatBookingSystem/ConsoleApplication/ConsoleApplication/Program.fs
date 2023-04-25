type Ticket = { seat:int; customer:string }

let mutable tickets = [ for n in 1..10 -> { seat = n; customer = "" } ]

let DisplayTickets () =
    printfn "%-5s %-15s" "Seat Number" "Customer Name"
    printfn "%-5s %-15s" "___________---_____________"
    for ticket in tickets do
        printfn "    %-5d      %-15s" ticket.seat ticket.customer

let BookSeat (customerName:string, seatNumber:int) =
    let assignCustomer (ticket:Ticket) =
        if ticket.seat = seatNumber then
            { ticket with customer = customerName }
        else
            ticket
    tickets <- List.map assignCustomer tickets

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

menu ()
