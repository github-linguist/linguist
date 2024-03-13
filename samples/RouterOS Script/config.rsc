# Author: @zachstence
# Date: 6/20/2022

:global StringVariable "StringVariable";

:local NumberVariable "NumberVariable";

:global ArrayVariable { 0; 1; 2; 3 };

:local ObjectVariable { "something"="value" };

:global ArrayOfObjects { \
    { "a"=1 ; "b"=2   }; \
    { "a"=3 ; "b"=4   }; \
    { "a"=5 ; "b"=6   }; \
    { "a"=7 ; "b"=8   }
};

:put "Loaded config:"
:environment print;
