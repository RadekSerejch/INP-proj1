-- cpu.vhd: Simple 8-bit CPU (BrainLove interpreter)
-- Copyright (C) 2021 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): DOPLNIT
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet ROM
   CODE_ADDR : out std_logic_vector(11 downto 0); -- adresa do pameti
   CODE_DATA : in std_logic_vector(7 downto 0);   -- CODE_DATA <- rom[CODE_ADDR] pokud CODE_EN='1'
   CODE_EN   : out std_logic;                     -- povoleni cinnosti
   
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(9 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- ram[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_WREN  : out std_logic;                    -- cteni z pameti (DATA_WREN='0') / zapis do pameti (DATA_WREN='1')
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA obsahuje stisknuty znak klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna pokud IN_VLD='1'
   IN_REQ    : out std_logic;                     -- pozadavek na vstup dat z klavesnice
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- pokud OUT_BUSY='1', LCD je zaneprazdnen, nelze zapisovat,  OUT_WREN musi byt '0'
   OUT_WREN : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

 -- zde dopiste potrebne deklarace signalu

 
--PC--
	signal pc_reg : std_logic_vector (11 downto 0);
	signal pc_inc : std_logic;
	signal pc_dec : std_logic;
--PC-- 

--PTR--
	signal ptr_reg : std_logic_vector (9 downto 0);
	signal ptr_inc : std_logic;
	signal ptr_dec : std_logic;
--PTR-- 

--CNT--
	signal cnt_cnt : std_logic_vector (7 downto 0);
	signal cnt_inc : std_logic;
	signal cnt_dec : std_logic;
--CNT--

--STAVY--
	type fsm_state is (
		s_start,
		s_fetch,
		s_decode,
		
		s_ptr_inc,
		s_ptr_dec,
		
		s_val_inc,
		s_val_dec,
		s_val_inc_mx,
		s_val_dec_mx,
		s_val_inc_end,
		s_val_dec_end,
		
		s_loop_start,
		s_loop_end,
		s_loop_break,
		s_loop_break_1,
		s_loop_break_2,
		s_loop_start_1,
		s_loop_start_2,
		s_loop_start_3,
		s_loop_end_1,
		s_loop_end_2,
		s_loop_end_3,
		s_loop_end_4,
		s_loop_end_1_cden,
		
		s_write,
		s_write_end,
		s_read,
		s_read_end,
		
		s_null
	);
	signal state : fsm_state := s_start;
	signal nstate : fsm_state;
--STAVY--
	
--MX--
	signal mx_select : std_logic_vector (1 downto 0) := (others => '0');
	signal mx_output : std_logic_vector (7 downto 0) := "00000000";
--MX--


begin

 -- zde dopiste vlastni VHDL kod

 -- pri tvorbe kodu reflektujte rady ze cviceni INP, zejmena mejte na pameti, ze 
 --   - nelze z vice procesu ovladat stejny signal,
 --   - je vhodne mit jeden proces pro popis jedne hardwarove komponenty, protoze pak
 --      - u synchronnich komponent obsahuje sensitivity list pouze CLK a RESET a 
 --      - u kombinacnich komponent obsahuje sensitivity list vsechny ctene signaly.
 
--PC--
	 pc: process (CLK, RESET, pc_inc, pc_dec) is
	 begin
		if RESET = '1' then
			pc_reg <= (others => '0');
		elsif rising_edge(CLK) then
			if pc_inc = '1' then 
				pc_reg <= pc_reg + 1;
			elsif pc_dec = '1' then
				pc_reg <= pc_reg - 1;
			end if;
		end if;
	 end process;
	 CODE_ADDR <= pc_reg;
--PC--

--CNT--
	cnt: process (CLK, RESET, cnt_inc, cnt_dec)is
	begin
		if RESET = '1' then
			cnt_cnt <= (others => '0');
		elsif rising_edge(CLK) then
			if cnt_inc = '1' then
				cnt_cnt <= cnt_cnt + 1;
			elsif cnt_dec = '1' then
				cnt_cnt <= cnt_cnt - 1;
			end if;
		end if;
	end process;
--CNT--

--PTR--
	ptr: process (CLK, RESET, ptr_inc, ptr_dec) is
	begin
		if RESET = '1' then
			ptr_reg <= (others => '0');
		elsif rising_edge(CLK) then
			if ptr_inc = '1' then
				ptr_reg <= ptr_reg + 1;
			elsif ptr_dec='1' then
				ptr_reg <= ptr_reg - 1;
			end if;		end if;
	end process;
	DATA_ADDR <= ptr_reg;
--PTR--

--MX--
	mux: process (CLK, RESET, mx_select) is
	begin
		if RESET = '1' then
			mx_output <= (others => '0');
		elsif rising_edge(CLK) then
			case mx_select is
				when "11" =>
					mx_output <= IN_DATA;
				when "10" =>
					mx_output <= DATA_RDATA + 1;
				when "01" =>
					mx_output <= DATA_RDATA - 1;
				when "00" =>
					mx_output <= DATA_RDATA;
				when others =>
					mx_output <= (others => '0');
			end case;
		end if;
	end process;
	DATA_WDATA <= mx_output;
--MX--


--FSM--
	state_logic: process (CLK, RESET, EN) is
	begin
		if RESET = '1' then
			state <= s_start;
		elsif rising_edge(CLK) then
			if EN = '1' then
				state <= nstate;
			end if;
		end if;
	end process;
	
	Fsm: process (state, OUT_BUSY, IN_VLD, CODE_DATA, DATA_RDATA, cnt_cnt) is
	begin
		--inicializace
		pc_inc <= '0';
		pc_dec <= '0';
		ptr_inc <= '0';
		ptr_dec <= '0';
		cnt_inc <= '0';
		cnt_dec <= '0';
		
		CODE_EN <= '0';
		DATA_EN <= '0';
		DATA_WREN <= '0';
		IN_REQ <= '0';
		OUT_WREN <= '0';
		
		mx_select <= "00";
		
		case state is
			when s_start =>
				nState <= s_fetch;
			when s_fetch =>
				CODE_EN <= '1';
				nState <= s_decode;
			when s_decode =>
				case CODE_DATA is
					when x"3E" =>
						nState <= s_ptr_inc;
					when x"3C" =>
						nState <= s_ptr_dec;
					when x"2B" =>
						nState <= s_val_inc;
					when x"2D" =>
						nState <= s_val_dec;
					when x"5B" =>
						nState <= s_loop_start;
					when x"5D" =>
						nState <= s_loop_end;
					when x"2E" =>
						nState <= s_write;
					when x"2C" =>
						nState <= s_read;
					when x"7E" =>
						nState <= s_loop_break;
					when x"00" =>
						nState <= s_null;
					when others =>
						pc_inc <= '1';
						nState <= s_fetch;
				end case;
				
			--posouvání pointeru--
			when s_ptr_inc =>
				pc_inc <= '1';
				ptr_inc <= '1';
				nState <= s_fetch;
			when s_ptr_dec =>
				pc_inc <= '1';
				ptr_dec <= '1';
				nState <= s_fetch;
			--posouvání pointeru--

			--inc hodnoty--
			when s_val_inc =>
				DATA_EN <= '1';
				DATA_WREN <= '0';
				nState <= s_val_inc_mx;
			when s_val_inc_mx =>
				mx_select <= "10";
				nState <= s_val_inc_end;
			when s_val_inc_end =>
				DATA_EN <= '1';
				DATA_WREN <= '1';
				pc_inc <= '1';
				nState <= s_fetch;
			--inc hodnoty--

			--dec hodnoty--
			when s_val_dec =>
				DATA_EN <= '1';
				DATA_WREN <= '0';
				nState <= s_val_dec_mx;
			when s_val_dec_mx =>
				mx_select <= "01";
				nState <= s_val_dec_end;
			when s_val_dec_end =>
				DATA_EN <= '1';
				DATA_WREN <= '1';
				pc_inc <= '1';
				nState <= s_fetch;
			--dec hodnoty--
			
			--vypsání hodnoty--
			when s_write =>
				DATA_EN <= '1';
				DATA_WREN <= '0';
				nState <= s_write_end;
			when s_write_end =>
				if OUT_BUSY /= '0' then
					DATA_EN <= '1';
					DATA_WREN <= '0';
					nState <= s_write_end;
				else
					OUT_DATA <= DATA_RDATA;
					OUT_WREN <= '1';
					pc_inc <= '1';
					nState <= s_fetch;
				end if;
			--vypsání hodnoty--
			
			--ètení hodnoty--
			when s_read =>
				IN_REQ <= '1';
				mx_select <= "11";
				nState <= s_read_end;
			when s_read_end =>
				if IN_VLD /= '1' then
					IN_REQ <= '1';
					mx_select <= "11";
					nState <= s_read_end;
				else
					DATA_EN <= '1';
					DATA_WREN <= '1';
					pc_inc <= '1';
					nState <= s_fetch;
				end if;
			--ètení hodnoty--
			
			--loop start--
			when s_loop_start =>
				pc_inc <= '1';
				DATA_EN <= '1';
				DATA_WREN <= '0';
				nState <= s_loop_start_1;
			when s_loop_start_1 =>
				--pokud pøeètu 0 => dojedu na konec všech vnoøených loopù
				if DATA_RDATA = "00000000" then
					cnt_inc <= '1';
					CODE_EN <= '1';
					nState <= s_loop_start_2;
				else
					nState <= s_fetch;
				end if;
			when s_loop_start_2 =>
				if cnt_cnt /= "00000000" then
					if CODE_DATA = x"5B" then
						cnt_inc <= '1';
					elsif CODE_DATA = x"5D" then
						cnt_dec <= '1';
					end if;
					pc_inc <= '1';
					nState <= s_loop_start_3;
				else
					nState <= s_fetch;
				end if;
			when s_loop_start_3 =>
				CODE_EN <= '1';
				nState <= s_loop_start_2;
				
			--loop start--
			
			--loop end--
			when s_loop_end =>
				DATA_WREN <= '0';
				DATA_EN <= '1';
				nState <= s_loop_end_1;
			when s_loop_end_1 =>
				if DATA_RDATA = "00000000" then
					pc_inc <= '1';
					nState <= s_fetch;
				else
					cnt_inc <= '1';
					pc_dec <= '1';
					nState <= s_loop_end_1_cden;
					
				end if;
			when s_loop_end_1_cden =>
					CODE_EN <= '1';
					nState <= s_loop_end_2;
			when s_loop_end_2 =>
				if cnt_cnt /= "00000000" then
					if CODE_DATA = x"5D" then
						cnt_inc <= '1';
					elsif CODE_DATA = x"5B" then
						cnt_dec <= '1';
					end if;
					nState <= s_loop_end_3;
				else
					nState <= s_fetch;
				end if;
			when s_loop_end_3 =>
				if cnt_cnt = "00000000" then
					pc_inc <= '1';
				else
					pc_dec <= '1';
					
				end if;
				nState <= s_loop_end_4;
			when s_loop_end_4 =>
					CODE_EN <= '1';
					nState <= s_loop_end_2;
			--loop end--
			
			--loop break --
			when s_loop_break =>
				cnt_inc <= '1';
				pc_inc <= '1';
				nState <= s_loop_break_1;
			when s_loop_break_1 =>
				if cnt_cnt /= "00000000" then
					CODE_EN <= '1';
					nState <= s_loop_break_2;
				else
					nState <= s_fetch;
				end if;
			when s_loop_break_2 =>
				if CODE_DATA = x"5B" then
					cnt_inc <= '1';
				elsif CODE_DATA = x"5D" then
					cnt_dec <= '1';
				end if;
				pc_inc <= '1';
				nState <= s_loop_break_1;
			--loop break --
			when s_null =>
				nState <= s_fetch;
		end case;
		
	end process;
--FSM--


end behavioral;
 
