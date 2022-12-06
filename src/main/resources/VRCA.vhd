library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity vrca is
    generic (
        width : positive := 32
    );
    port (
        a, b  : in std_logic_vector(width-1 downto 0);
        cin   : in std_logic;
        s     : out std_logic_vector(width-1 downto 0);
        cout  : out std_logic
    );
end entity;

architecture rtl of vrca is
    signal carry, sum : std_logic_vector(width downto 0);
begin
    carry(width downto 1) <= (others => '0'); carry(0) <= cin;
    sum   <= std_logic_vector(unsigned(a) + unsigned(b) + unsigned(carry));
    
    s    <= sum(width-1 downto 0);
    cout <= sum(width);
end architecture;
