<%

class Set extends Enumerable {
    def init(ary = []) {
        @items = {};
        throw TypeError.new("Expected Array in Set initializer") unless ary.is_an(Array);
        for item in ary {
            @items[item] = true;
        }
    }

    def items {
        @items.keys;
    }

    def add(item) {
        retn = @items.has_key(item);
        @items[item] = true;
        !retn;
    }

    def delete(item) {
        retn = @items.has_key(item);
        @items.delete(item);
        retn;
    }

    def has(item) {
        @items.has_key(item);
    }

    def length {
        @items.length;
    }

    def inspect {
        if @items.any {
            "#<Set:{ " + @items.map(\(kv) { kv[0].inspect }).join(", ") + " }>";
        } else {
            "#<Set:{}>";
        }
    }

    def enumerate {
        self.items.enumerate;
    }

    def ==(other) {
        if other.is_a(Set) {
            @items == other._dict;
        } else {
            false;
        }
    }

    def _dict {
        @items;
    }
}
