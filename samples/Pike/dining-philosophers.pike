class Philosopher
{
    string name;
    object left;
    object right;

    void create(string _name, object _left, object _right)
    {
        name = _name;
        left = _left;
        right = _right;
    }

    void take_forks()
    {
        if (left->take(this) && right->take(this))
        {
            write("%s is EATING\n", name);
            call_out(drop_forks, random(30));
        }
        else
        {
            write("%s is WAITING\n", name);
            if (random(10) >= 8)
                drop_forks();
            call_out(take_forks, random(10));
        }
    }

    void drop_forks()
    {
        left->drop(this);
        right->drop(this);
        write("%s is THINKING\n", name);
        call_out(take_forks, random(30));
    }
}

class Fork
{
    int number;
    Philosopher user;

    void create(int _number)
    {
        number = _number;
    }

    int take(object new_user)
    {
        if (!user)
        {
            write("%s takes fork %d\n", new_user->name, number);
            user = new_user;
            return 1;
        }
        else if (new_user == user)
        {
            write("%s has fork %d\n", new_user->name, number);
            return 1;
        }
        else
            write("%s tries to take fork %d from %s\n", new_user->name, number, user->name);
    }

    void drop(object old_user)
    {
        if (old_user == user)
        {
            write("%s drops fork %d\n", old_user->name, number);
            user = 0;
        }
    }
}

int main(int argc, array argv)
{

  array forks = ({ Fork(1), Fork(2), Fork(3), Fork(4), Fork(5) });
  array philosophers = ({
                           Philosopher("einstein", forks[0], forks[1]),
                           Philosopher("plato", forks[1], forks[2]),
                           Philosopher("sokrates", forks[2], forks[3]),
                           Philosopher("chomsky", forks[3], forks[4]),
                           Philosopher("archimedes", forks[4], forks[0]),
                        });

  call_out(philosophers[0]->take_forks, random(5));
  call_out(philosophers[1]->take_forks, random(5));
  call_out(philosophers[2]->take_forks, random(5));
  call_out(philosophers[3]->take_forks, random(5));
  call_out(philosophers[4]->take_forks, random(5));
  return -1;
}
