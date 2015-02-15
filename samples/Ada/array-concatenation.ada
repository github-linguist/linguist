type T is array (Positive range <>) of Integer;
X : T := (1, 2, 3);
Y : T := X & (4, 5, 6); -- Concatenate X and (4, 5, 6)
