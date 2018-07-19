#include <string>
#include <set>
#include <list>
#include <map>
#include <iostream>


struct Employee
{
	std::string Name;
	std::string ID;
	unsigned long Salary;
	std::string Department;
	Employee(std::string _Name = "", std::string _ID = "", unsigned long _Salary = 0, std::string _Department = "")
	: Name(_Name), ID(_ID), Salary(_Salary), Department(_Department)
	{ }

	void display(std::ostream& out) const
	{
		out << Name << "\t" << ID << "\t" << Salary << "\t" << Department << std::endl;
	}
};

// We'll tell std::set to use this to sort our employees.
struct CompareEarners
{
	bool operator()(const Employee& e1, const Employee& e2)
	{
		return (e1.Salary > e2.Salary);
	}
};

// A few typedefs to make the code easier to type, read and maintain.
typedef std::list<Employee> EMPLOYEELIST;

// Notice the CompareEarners; We're telling std::set to user our specified comparison mechanism
// to sort its contents.
typedef std::set<Employee, CompareEarners> DEPARTMENTPAYROLL;

typedef std::map<std::string, DEPARTMENTPAYROLL> DEPARTMENTLIST;

void initialize(EMPLOYEELIST& Employees)
{
	// Initialize our employee list data source.
	Employees.push_back(Employee("Tyler Bennett", "E10297", 32000, "D101"));
	Employees.push_back(Employee("John Rappl", "E21437", 47000, "D050"));
	Employees.push_back(Employee("George Woltman", "E21437", 53500, "D101"));
	Employees.push_back(Employee("Adam Smith", "E21437", 18000, "D202"));
	Employees.push_back(Employee("Claire Buckman", "E39876", 27800, "D202"));
	Employees.push_back(Employee("David McClellan", "E04242", 41500, "D101"));
	Employees.push_back(Employee("Rich Holcomb", "E01234", 49500, "D202"));
	Employees.push_back(Employee("Nathan Adams", "E41298", 21900, "D050"));
	Employees.push_back(Employee("Richard Potter", "E43128", 15900, "D101"));
	Employees.push_back(Employee("David Motsinger", "E27002", 19250, "D202"));
	Employees.push_back(Employee("Tim Sampair", "E03033", 27000, "D101"));
	Employees.push_back(Employee("Kim Arlich", "E10001", 57000, "D190"));
	Employees.push_back(Employee("Timothy Grove", "E16398", 29900, "D190"));
}

void group(EMPLOYEELIST& Employees, DEPARTMENTLIST& Departments)
{
	// Loop through all of our employees.
	for( EMPLOYEELIST::iterator iEmployee = Employees.begin();
		 Employees.end() != iEmployee;
		 ++iEmployee )
	{
		DEPARTMENTPAYROLL& groupSet = Departments[iEmployee->Department];

		// Add our employee to this group.
		groupSet.insert(*iEmployee);
	}
}

void present(DEPARTMENTLIST& Departments, unsigned int N)
{
	// Loop through all of our departments
	for( DEPARTMENTLIST::iterator iDepartment = Departments.begin();
		 Departments.end() != iDepartment;
		 ++iDepartment )
	{
		std::cout << "In department " << iDepartment->first << std::endl;
		std::cout << "Name\t\tID\tSalary\tDepartment" << std::endl;
		// Get the top three employees for each employee
		unsigned int rank = 1;
		for( DEPARTMENTPAYROLL::iterator iEmployee = iDepartment->second.begin();
			 ( iDepartment->second.end() != iEmployee) && (rank <= N);
			 ++iEmployee, ++rank )
		{
			iEmployee->display(std::cout);
		}
		std::cout << std::endl;
	}
}

int main(int argc, char* argv[])
{
	// Our container for our list of employees.
	EMPLOYEELIST Employees;

	// Fill our list of employees
	initialize(Employees);

	// Our departments.
	DEPARTMENTLIST Departments;

	// Sort our employees into their departments.
	// This will also rank them.
	group(Employees, Departments);

	// Display the top 3 earners in each department.
	present(Departments, 3);

	return 0;
}
