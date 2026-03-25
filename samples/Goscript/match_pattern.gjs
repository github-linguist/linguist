// Exemple réel: Pattern matching
enm Status {
    Active,
    Inactive,
    Pending,
    Deleted
}

fn process_status(status: Status) -> string {
    match status {
        Active => "User is active",
        Inactive => "User is inactive",
        Pending => "Waiting for approval",
        Deleted => "Account removed"
    }
}

fn main() {
    lt statuses = [Active, Pending, Inactive, Deleted]
    
    for s in statuses {
        println(process_status(s))
    }
    
    ret 0
}
