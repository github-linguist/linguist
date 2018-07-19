Employee = Struct.new(:name, :employee_id, :salary, :department)

employees = [
    Employee.new("Tyler Bennett",   "E10297", 32000, "D101"),
    Employee.new("John Rappl",      "E21437", 47000, "D050"),
    Employee.new("George Woltman",  "E00127", 53500, "D101"),
    Employee.new("Adam Smith",      "E63535", 18000, "D202"),
    Employee.new("Claire Buckman",  "E39876", 27800, "D202"),
    Employee.new("David McClellan", "E04242", 41500, "D101"),
    Employee.new("Rich Holcomb",    "E01234", 49500, "D202"),
    Employee.new("Nathan Adams",    "E41298", 21900, "D050"),
    Employee.new("Richard Potter",  "E43128", 15900, "D101"),
    Employee.new("David Motsinger", "E27002", 19250, "D202"),
    Employee.new("Tim Sampair",     "E03033", 27000, "D101"),
    Employee.new("Kim Arlich",      "E10001", 57000, "D190"),
    Employee.new("Timothy Grove",   "E16398", 29900, "D190"),
]

def show_top_salaries_per_group(groups, n)
  groups.each do |dept, emps|
    puts dept
    # sort by salary descending
    emps.sort_by {|emp| -emp.salary}.first(n).each do |e|
      puts "    %-16s %6s %7d" % [e.name, e.employee_id, e.salary]
    end
    puts
  end
end

groups = employees.group_by {|emp| emp.department}.sort

show_top_salaries_per_group(groups,3)
