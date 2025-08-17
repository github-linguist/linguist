// unionable types
public <int|string*>* testUnionable() {
    <int|object> tmp = 0;
    return ({ 1, ({ "a" }) });
}
