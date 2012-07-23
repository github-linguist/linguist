-- VHDL example file

library ieee;
use ieee.std_logic_1164.all;

entity inverter is
	port(a : in std_logic;
	     b : out std_logic);
end entity;

architecture rtl of inverter is
begin
	b <= not a;
end architecture;
