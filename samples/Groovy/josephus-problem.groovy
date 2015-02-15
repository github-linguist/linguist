int[] Josephus (int size, int kill, int survivors) {
    // init user pool
    def users = new int[size];

    // give initial values such that [0] = 1 (first person) [1] = 2 (second person) etc
    users.eachWithIndex() {obj, i -> users[i] = i + 1};

    // keep track of which person we are on (ranging from 1 to kill)
    def person = 1;

    // keep going until we have the desired number of survivors
    while (users.size() > survivors)
    {
        // for each person, if they are the kill'th person, set them to -1 to show eliminated
        users.eachWithIndex() {obj, i ->
            if (person++ % kill == 0) {
                users[i] = -1;
            }

            // if person overflowed kill then reset back to 1
            if (person > kill) {person = 1;}
        }

        // clear out all eliminated persons
        users = users.findAll{w -> w >= 0};
    }

    // resulting set is the safe positions
    return users;
}

// Run some test cases

println "Final survivor for n = 10201 and k = 17: " + Josephus(10201,17,1)[0];

println "4 safe spots for n = 10201 and k = 17: " + Josephus(10201,17,4);
