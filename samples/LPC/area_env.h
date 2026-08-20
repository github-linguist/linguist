int minLevel;

public int query_minLevel() {
  return minLevel;
}
public void set_minLevel(int level) {
  minLevel = level;
}

public string query_label() {
  return "Area Label";
}

public string canEnterArea() {  
  return this_player()->query_level() < minLevel ? "You are not experienced enough to enter this area." : 0;
}
