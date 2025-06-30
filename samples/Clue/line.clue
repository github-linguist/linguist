static dots, formula, errormsg;
dots = {0, 0, 0, 0};
formula = "y=";
errormsg = "";

local utf8 = require("utf8");
local env = {x = 0, y = 0};

for k, v of math {
    env[k] = v;
}

method love.textinput(key) {
    formula ..= key;
    dots = {};
    try {
        local formula, errormsg = loadstring(formula);
        if !formula {error(errormsg);}
        setfenv(formula, env);
        for x = -150, 650 {
            env.x = x;
            formula();
            table.insert(dots, env.x || 0);
            table.insert(dots, -(env.y || 0));
        }
    } catch error {
        dots = nil;
        errormsg = error;
    }
}

method love.keypressed(key) {
    if key == "backspace" && utf8.len(formula) > 0 {
        formula = ($)::sub(1, utf8.len($)-1);
    }
}
