
-- Clause de contexte: paquetages
with Text_IO;                     use Text_IO;
with Ada.Integer_Text_IO;         use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;           use Ada.Float_Text_IO;
with Ada.Numerics.Float_Random;   use Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;

procedure puissance_4 is  -- partie déclarative
   
   	-- instanciation d''un paquetage générique de fonctions
   
   	package Math is new Ada.Numerics.Generic_Elementary_Functions(Float); use Math;

   	type Etat is (circle,cross,free);

	subtype T_indice_colonne is Integer range 0..6;
	subtype T_indice_line is Integer range 0..5;

	type T_board is array(Integer range<>, integer range<>) of Etat;
	type T_available is array(Integer range<>) of Integer;
	type T_pourcentage is array(0..5) of integer;
	type T_array_ia is array(0..1,0..5) of integer;
	type T_array_all is array(T_indice_colonne,0..1,T_indice_line) of integer;

	function change_state(st : Etat) return Etat is
		st_tmp : Etat;
	begin
		if st = circle then
			st_tmp := cross;
		elsif st = cross then
			st_tmp := circle;
		end if;

		return st_tmp;
	end change_state;

	procedure Put(st : in out  Etat) is
	begin
		case st is
		   	when circle => Put('O');
		   	when free => Put('.');
		   	when cross => Put('X');
		end case;
	end Put;

	procedure Put(t : in out T_board) is
	begin
		for i in t'range(1) loop
			new_line;
	    	for y in t'range(2) loop
	        	Put("  "); Put(T(I,Y));
	    	end loop;
	    	new_line;
	  	end loop;
	  	Put(" ");
	  	for i in t'range(2) loop
  			Put("---");
	  	end loop;
	  	new_line;
	  	for i in t'range(2) loop
  			Put(" " & integer'image(i));
	  	end loop;
	  	new_line;
	end Put;

	procedure game_during(game : in out boolean; t : in out  T_board) is
		bool_tmp : boolean := false;
	begin
		for i in t'range(2) loop
			if(t(0,i) = free) then
				bool_tmp := true;
			end if;
		end loop;

		if not bool_tmp then
			Put_line("Toutes les cases du tableau ont été remplies");
		end if;
		
		game := bool_tmp;
	end game_during;


	function neigbours(		tmp_direction_x, tmp_direction_y	: in integer; 
							jeton_y                  			: in T_indice_line; 
							jeton_x                				: in out T_indice_colonne;
							t                        			: in out T_board;
							state                    			: in out Etat) return integer is

		direction_x 	: integer := tmp_direction_x;
		direction_y 	: integer := tmp_direction_y;
		pos_x 			: integer;
		pos_y	 		: integer;
		value_return 	: integer := 0;
	begin
		for i in 0..1 loop
			pos_x := jeton_x;
			pos_y := jeton_y;

			while pos_x + direction_x in t'range(2) and pos_y + direction_y in t'range(1) loop
				pos_x := pos_x + direction_x;
				pos_y := pos_y + direction_y;

				if t(pos_y, pos_x) = state then
					value_return := value_return + 1;	
				else
					exit;
				end if;
			end loop;

			direction_x := direction_x * (-1);
			direction_y := direction_y * (-1);
		end loop;
		return value_return;
	end neigbours;

	procedure check_win( line 	: in out T_indice_line; 
						col     : in out T_indice_colonne;
						t       : in out T_board;
						state   : in out Etat;
						b_win   : in out boolean;
						nb_win  : in positive) is

		i_win : integer := 1;
	begin
		for i in 0..1 loop
			for j in -1..1 loop
				i_win := 1;
				if ((i = 0) and (j = 1)) or (i = 1) then		
	   				i_win := i_win + neigbours(i, j, line, col, t, state);
					b_win := i_win >= nb_win;
				end if;
				exit when b_win ;
			end loop;
			exit when b_win ;
	   	end loop;
	end check_win;

	function check_neigbours( 	line 	: T_indice_line; 
								col     : T_indice_colonne;
								Board   : T_board;
								st   	: Etat) return T_array_ia is
								
		state_mine 				: Etat := st;
		state_his 				: Etat := change_state(st);
		t 						: T_board := Board;
		jeton_x 				: integer := col;
		jeton_y 				: integer := line;
		value_return_mine 		: integer := 0;
		value_return_his 		: integer := 0;
		a_tmp_his 				: T_pourcentage := (others=>-1);
		a_tmp_mine 				: T_pourcentage := (others=>-1);
		count 					: integer := 2;
		nb_jeton_neignours_his 	: integer := 0;
		nb_jeton_neignours_mine	: integer := 0;
		a_ia 					: T_array_ia;
	begin
		for i in 0..1 loop
			for j in -1..1 loop
				if ((i = 0) and (j = 1)) or (i = 1) then
					nb_jeton_neignours_his := neigbours(i, j, jeton_y, jeton_x, t, state_his);
					nb_jeton_neignours_mine := neigbours(i, j, jeton_y, jeton_x, t, state_mine);

					a_tmp_his(count) := nb_jeton_neignours_his;
					a_tmp_mine(count) := nb_jeton_neignours_mine;

	   				value_return_his := value_return_his + nb_jeton_neignours_his;
	   				value_return_mine := value_return_mine + nb_jeton_neignours_mine;

	   				count := count + 1;
				end if;
			end loop;
	   	end loop;

	   	a_tmp_his(0) := col;
	   	a_tmp_mine(0) := col;
	   	a_tmp_his(1) := value_return_his;
	   	a_tmp_mine(1) := value_return_mine;

		for i in a_tmp_his'range loop
			a_ia(0,i) := a_tmp_mine(i);
			a_ia(1,i) := a_tmp_his(i);
		end loop;
	   	
	   	return a_ia;
	end check_neigbours;

	function check_col(t : T_board; col : integer) return boolean is
		i : integer := 0;
		line : integer := -1;
	begin
		while t(i, col) = free loop
	  		line := i;
	  		i := i+1;

	  		if(i > 5) then
	  			line := 5;	
	  			exit;
	  		end if;
	  	end loop;

	  	if line = -1 then
	  		Put_line("false");
	  		return false;
	  	else
	  		Put_line("True");
	  		return true;
	  	end if;
	end;

	procedure add_jeton(	t     	: in out  T_board; 
							state 	: in out Etat; 
							col  	: in out T_indice_colonne;
							nb_win 	: in positive;
							b_win 	: in out boolean) is
		i    : integer := 0;
		line : integer := -1;
	begin
	  	while t(i, col) = free loop
	  		
	  		line := i;
	  		i := i+1;

	  		if(i > 5) then
	  			line := 5;	
	  			exit;
	  		end if;
	  	end loop;

  		t(line, col) := state;
  		check_win(line, col, t, state, b_win, nb_win);
  		Put(t);
	end add_jeton;

	function check_available_case(t : T_board) return T_available is
		a_available : T_available(T_indice_colonne) := (others=>-1);
		line : integer := -1;
		i : integer := 0;
	begin
		for col in t'range(2) loop
			i := 0;
			while t(i, col) = free loop
		  		line := i;
		  		i := i+1;

		  		if(i > 5) then
		  			line := 5;	
		  			exit;
		  		end if;
  			end loop;

  			a_available(col) := line;
		end loop;

		return a_available;
	end;

	procedure swap_array_items(indice_1, indice_2 : in integer; t : in out T_array_all) is
		a_tmp : T_array_all;
	begin
		for i in t'range(2) loop
			for y in t'range(3) loop
				a_tmp(0,i,y) := t(indice_1,i,y);
				t(indice_1,i,y) := t(indice_2,i,y);
				t(indice_2,i,y) := a_tmp(0,i,y);
			end loop;
		end loop;
	end;

	procedure sort_array(count : integer; t : in out T_array_all; mode : integer := 0) is
		ind : natural := 0;
		tmp_mine : integer := -10;
		tmp_his : integer := -10; 
	begin
		if count < t'last(1) then
			for i in count .. t'last(1) loop
				for j in t'range(2) loop
					for y in 2 .. 5 loop
						if t(i,j,y) > tmp_his then
							tmp_his := t(i,j,y);
							if mode = 0 then
								ind := i;
							end if;
						end if;	
					end loop;
					if mode /= 0 then
						for y in 1..5 loop
							if t(i,j,y) = tmp_his then
								if t(i,j,1) >= tmp_mine and j=0 then
									tmp_mine := t(i,j,1);
									ind := i;
								elsif t(i,j,1) > tmp_mine then
									tmp_mine := t(i,j,1);
									ind := i;
								end if;	
							end if;
						end loop;
					end if;	
				end loop;
			end loop;	
			swap_array_items(count, ind, t);

			sort_array(count + 1, t);
		end if;

	end;

	procedure show_array(a_all : T_array_all) is
	begin
		for i in a_all'range(1) loop
			for j in a_all'range(2) loop
				for k in a_all'range(3) loop	
					Put("Indice I : " & integer'image(I) & " / Indice J : " & integer'image(J) & " / Indice k : " & integer'image(I) & " Valeure : ");Put(a_all(i,j,k));new_line;
				end loop;
					Put_line("-");
			end loop;
			Put_line("--------------------");
		end loop;
	end;

	function choose_case(t : T_board; a_available : T_available; state : Etat) return integer is
		result : integer := -1;
		a_ia : T_array_ia;
		a_all : T_array_all;
		b_loop : boolean := true;
	begin
		for i in a_available'range loop
			if (a_available(i) > -1) then
				a_ia := check_neigbours(a_available(i), i, t, state);
				for j in a_ia'range(1) loop
					for k in a_ia'range(2) loop
						a_all(i,j,k) := a_ia(j,k);	
					end loop;	
				end loop;
			else
				for k in a_ia'range(2) loop
					a_all(i,0,k) := 0;	
					a_all(i,1,k) := 0;	
				end loop;
			end if;
		end loop;

		sort_array(0, a_all);

		sort_array(0, a_all,1);

		for i in a_all'range(1) loop
			for k in 2 .. 5 loop
			if b_loop then

				if (a_all(i,0,k) = 3) then
					result := a_all(i,0,0);
					b_loop := false;

				elsif (a_all(i,1,k) = 3) then
					result := a_all(i,1,0);
					b_loop := false;

				else
					result := a_all(0,1,0);
				end if;
			end if;	
			end loop;	
		end loop;

		return result;
	end;

	Board  		: T_board(T_indice_line,T_indice_colonne) := (others=>(others=>free));
	state  		: Etat;
	game   		: boolean := true;
	b_win  		: boolean := false;
	nb_win 		: positive := 4;
	error  		: integer := 0;
	b_ia   		: boolean := false;
	b_answer 	: boolean := false;
	nb_players	: natural := 0;
	col 		: integer := -1;
	tmp 		: integer;

begin -- corps de la procédure principale
	state := cross;

	while not b_answer loop
		if nb_players = 0 then
			Put_line("Voulez vous jouer seul ou a deux ? ");
		end if;
		
		Put_line("Répondez par 1 ou 2");
		Get(nb_players);

		if nb_players > 0 and nb_players < 3 then
			b_answer := true;
		else
			Put_line("Veuillez entrer une valeur valide");
		end if;
	end loop;

	for i in 0 .. 41 loop

		if(not b_win) then
	   		Put("au tour de ");Put(state);new_line;

			if (nb_players = 2) or (nb_players = 1 and state = cross) then
				col := -1;

		   		while col = -1 loop

		   			if error > -1 then
						Put("Veuillez entrer la colonne dans laquelle vous voulez insérer votre jeton : ");
					end if;

					Get(col);

					if(col >= 0) and (col <= 6) then
						if(check_col(Board, col)) then
							add_jeton(Board, state, col, nb_win, b_win);

							if( not b_win) then
								state := change_state(state);
							end if;
							error := 0;
						else
							Put("Veuillez entrer une autre colonne : ");
							error := -2;
						end if;
					else
						Put("Veuillez entrer une valeure entre 0 et 6 compris svp : ");
						col := -1;
						error := -1;
					end if;

					if b_win then
						col := 6;	
						exit;
					end if;
		   		end loop;
			else
				tmp := choose_case(Board, check_available_case(Board), circle);
				add_jeton(Board, state, tmp, nb_win, b_win);

				if not b_win then
					state := change_state(state);
				end if;
		   	end if;
	   	end if;	
	end loop;

   	if b_win then
		Put(state);Put(" Gagné");new_line;new_line;new_line;
	else
		Put(" Egalité");
	end if;
end puissance_4;