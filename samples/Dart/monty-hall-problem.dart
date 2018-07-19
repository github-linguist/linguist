int rand(int max) => (Math.random()*max).toInt();

class Game {
  int _prize;
  int _open;
  int _chosen;

  Game() {
    _prize=rand(3);
    _open=null;
    _chosen=null;
  }

  void choose(int door) {
    _chosen=door;
  }

  void reveal() {
    if(_prize==_chosen) {
      int toopen=rand(2);
      if (toopen>=_prize)
        toopen++;
      _open=toopen;
    } else {
      for(int i=0;i<3;i++)
        if(_prize!=i && _chosen!=i) {
          _open=i;
          break;
        }
    }
  }

  void change() {
    for(int i=0;i<3;i++)
      if(_chosen!=i && _open!=i) {
        _chosen=i;
        break;
      }
  }

  bool hasWon() => _prize==_chosen;

  String toString() {
    String res="Prize is behind door $_prize";
    if(_chosen!=null) res+=", player has chosen door $_chosen";
    if(_open!=null) res+=", door $_open is open";
    return res;
  }
}

void play(int count, bool swap) {
  int wins=0;

  for(int i=0;i<count;i++) {
    Game game=new Game();
    game.choose(rand(3));
    game.reveal();
    if(swap)
      game.change();
    if(game.hasWon())
      wins++;
  }
  String withWithout=swap?"with":"without";
  double percent=(wins*100.0)/count;
  print("playing $withWithout switching won $percent%");
}

test() {
  for(int i=0;i<5;i++) {
    Game g=new Game();
    g.choose(i%3);
    g.reveal();
    print(g);
    g.change();
    print(g);
    print("win==${g.hasWon()}");
  }
}

main() {
  play(10000,false);
  play(10000,true);
}
