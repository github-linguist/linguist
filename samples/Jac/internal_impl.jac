obj GradeBook {
    has students: dict[str, list[int]] = {};

    def add_student(name: str) -> None;
    def add_grade(student: str, grade: int) -> None;
    def get_average(student: str) -> float;
    def get_all_averages() -> dict[str, float];
}

impl GradeBook.add_student(name: str) -> None {
    if name not in self.students {
        self.students[name] = [];
        print(f"Added student: {name}");
    } else {
        print(f"Student {name} already exists");
    }
}

impl GradeBook.add_grade(student: str, grade: int) -> None {
    if grade < 0 or grade > 100 {
        print(f"Invalid grade: {grade}");
        return;
    }

    if student in self.students {
        self.students[student].append(grade);
        print(f"Added grade {grade} for {student}");
    } else {
        print(f"Student {student} not found");
    }
}

impl GradeBook.get_average(student: str) -> float {
    if student not in self.students or len(self.students[student]) == 0 {
        return 0.0;
    }
    grades = self.students[student];
    return sum(grades) / len(grades);
}

impl GradeBook.get_all_averages() -> dict[str, float] {
    averages: dict[str, float] = {};
    for (student, grades) in self.students.items() {
        if len(grades) > 0 {
            averages[student] = sum(grades) / len(grades);
        }
    }
    return averages;
}

with entry {
    # Create gradebook
    gradebook = GradeBook();

    # Add students
    gradebook.add_student("Alice");
    gradebook.add_student("Bob");

    # Add grades
    gradebook.add_grade("Alice", 88);
    gradebook.add_grade("Alice", 92);
    gradebook.add_grade("Bob", 85);
    gradebook.add_grade("Bob", 79);

    # Get results
    all_averages = gradebook.get_all_averages();
    for (student, avg) in all_averages.items() {
        letter = "A" if avg >= 90 else "B" if avg >= 80 else "C" if avg >= 70 else "F";
        print(f"{student}: {avg} ({letter})");
    }
}
