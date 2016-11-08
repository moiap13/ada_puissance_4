
-- **********************************************
-- *											*
-- * Author 	: Antonio Pisanello				*
-- * Version 	: 1.0							*
-- * Date 		: Tuesday, november 8th 2016 	*
-- * 											*
-- **********************************************

-- Clause de contexte: paquetages
with Text_IO;                     	use Text_IO;
with Ada.Integer_Text_IO;         	use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;           	use Ada.Float_Text_IO;
with Ada.Numerics.Float_Random;   	use Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;

procedure puissance_4_gui is  -- partie déclarative
	
	-- Types
   	type Etat is (circle,cross,free); -- type énumérés des différents états possibles dans le tableau

	subtype T_indice_colonne is Integer range 0..6;
	subtype T_indice_line is Integer range 0..5;

	type T_board is array(Integer range<>, integer range<>) of Etat; -- type du tableau de jeu
	type T_available is array(Integer range<>) of Integer;
	type T_array_ia is array(0..1,0..5) of integer;
	type T_array_all is array(T_indice_colonne,0..1,0..5) of integer; -- utilisé par l'IA pour trouver la meilleure case

	-- Fonctions

	-- fonction qui change l'etat donné en paramètre
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

	-- réécriture de la fonction Put afin de pouvoir afficher le type Etat
	procedure Put(st : in out  Etat) is
	begin
		case st is
		   	when circle => Put('O');
		   	when free => Put('.');
		   	when cross => Put('X');
		end case;
	end Put;

	-- réécriture de la fonction Put afin de pouvoir afficher le tableau de jeu
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

	-- fonction qui vérifie les cases à côtés d'une case donée en paramètre afin de déteminer combien 
	-- de cases "voisines" ont le même Etat que celui qui est en jeu
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
		for i in 0..1 loop -- for de deux car on fait deux fois le meme calcul une fois dans un sens et une seconde fois dans l'autre
			pos_x := jeton_x;
			pos_y := jeton_y;

			while pos_x + direction_x in t'range(2) and pos_y + direction_y in t'range(1) loop
				pos_x := pos_x + direction_x;
				pos_y := pos_y + direction_y;

				if t(pos_y, pos_x) = state then -- si la case voisine est de meme etat que nous
					value_return := value_return + 1; -- on incrémente le nombre de cases é coté qui sont les mêmes que nous
				else
					exit;
				end if;
			end loop;

			direction_x := direction_x * (-1);
			direction_y := direction_y * (-1); -- permet de repartir dans l'autre sense
		end loop;
		return value_return;
	end neigbours;

	-- procedure qui permet de savoir si une case permet d'aligner 4 jetons
	procedure check_win( line 	: in out T_indice_line; 
						col     : in out T_indice_colonne;
						t       : in out T_board;
						state   : in out Etat;
						b_win   : in out boolean;
						nb_win  : in positive) is

		i_win : integer; 
	begin
		for i in 0..1 loop
			for j in -1..1 loop
				i_win := 1;
				if ((i = 0) and (j = 1)) or (i = 1) then		
	   				i_win := i_win + neigbours(i, j, line, col, t, state); -- on ajoute a i_win le nombre de voisins
					b_win := i_win >= nb_win; -- on stocke le test
				end if;
				exit when b_win ;
			end loop;
			exit when b_win ;
	   	end loop;
	end check_win;

	-- fonction utilisée pour l'IA elle retourne un tableau trié qui donne la meilleure case 
	-- en fonction des jetons que l'rdinateur aligne et ceux qu'ils bloque
	-- ce tableau est constitué des nombre de jetons que l'ordi bloque et qu'il aligne ainsi
	-- que du nombre de jeton qu'il bloque ou aligne en une ligne 
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
		a_tmp_his 				: array(0..5) of integer := (others=>-1);
		a_tmp_mine 				: array(0..5) of integer := (others=>-1);
		count 					: integer := 2;
		nb_jeton_neignours_his 	: integer := 0;
		nb_jeton_neignours_mine	: integer := 0;
		a_ia 					: T_array_ia;
	begin
		for i in 0..1 loop
			for j in -1..1 loop
				if ((i = 0) and (j = 1)) or (i = 1) then
					nb_jeton_neignours_his := neigbours(i, j, jeton_y, jeton_x, t, state_his); -- on récupère le nombre de voisins pour l'Etat du joueur pour une ligne
					nb_jeton_neignours_mine := neigbours(i, j, jeton_y, jeton_x, t, state_mine); -- on récupère le nombre de voisins pour l'Etat de l'IA pour une ligne

					a_tmp_his(count) := nb_jeton_neignours_his; -- on le stocke dans un tableau
					a_tmp_mine(count) := nb_jeton_neignours_mine; -- on le stocke dans un tableau

	   				value_return_his := value_return_his + nb_jeton_neignours_his; -- on ajoute le nombre de jeton totaux bloqués
	   				value_return_mine := value_return_mine + nb_jeton_neignours_mine; -- on ajoute le nombre de jeton totaux alignés

	   				count := count + 1;
				end if;
			end loop;
	   	end loop;

	   	a_tmp_his(0) := col;
	   	a_tmp_mine(0) := col; -- on indique la colonne 
	   	a_tmp_his(1) := value_return_his;
	   	a_tmp_mine(1) := value_return_mine; -- on stoque le nombre de jeton totaux dans le tableau

		for i in a_tmp_his'range loop
			a_ia(0,i) := a_tmp_mine(i);
			a_ia(1,i) := a_tmp_his(i); -- on crée le tableau a_ia qui sera utilisé par l'IA
		end loop;
	   	
	   	return a_ia;
	end check_neigbours;

	-- fonction qui verifie pour une colonne la premiere ligne vide qui permet 
	-- d'acceullir un jeton utilisée pour etre sur qu'un joueur utilise une bonne colonne 
	-- avant de mettre un jeton => si la colone est remplie le joueur ne peut pas mettre son jeton
	function check_col(t : T_board; col : integer) return boolean is
		i 		: integer := 0;
		line 	: integer := -1; -- de base on considère la colonne comme remplie
	begin
		while t(i, col) = free loop
	  		line := i; -- si la colonne n'est pas remplis on prend la dernière case possible
	  		i := i+1; 

	  		if(i > 5) then
	  			line := 5; -- si la colonne est vide on donne la valeur de la dernière ligne
	  			exit;
	  		end if;
	  	end loop;

	  	if line = -1 then
	  		return false;
	  	else
	  		return true;
	  	end if;
	end;

	-- procedure qui rajoute un jeton dans le tableau
	procedure add_jeton(	t     	: in out  T_board; 
							state 	: in out Etat; 
							col  	: in out T_indice_colonne;
							nb_win 	: in positive;
							b_win 	: in out boolean) is
		i    : integer := 0;
		line : integer := -1;
	begin
	  	while t(i, col) = free loop -- on vérifie que la colonne est accéptable
	  		
	  		line := i; -- on en déduis la variable line
	  		i := i+1;

	  		if(i > 5) then
	  			line := 5;	
	  			exit;
	  		end if;
	  	end loop;

  		t(line, col) := state; -- on ajoute l'etat dans le tableau
  		check_win(line, col, t, state, b_win, nb_win); -- on verifie si en posant ce jeton le joueur a gagné
  		Put(t);
	end add_jeton;

	-- Procedure utilisée pour l'IA qui vérifie les cases pouvant acceuillir un jeton
	-- le tableau retournée sera utilisé afon de verifier pour chaques cases possible
	-- la meilleure cases a utilisé pour gagné ou bloquer le joueur
	function check_available_case(t : T_board) return T_available is
		a_available : T_available(T_indice_colonne) := (others=>-1);
		line 		: integer := -1;
		i 			: integer := 0;
	begin
		for col in t'range(2) loop
			i := 0;
			line := -1;
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

		return a_available; -- on retourne un tableau qui utilise les indices pour représenté les colonnes
							-- et pour un indice on stoque la dernière ligne accéssible
	end;

	-- procedure utilisée lors du tri séléctif elle permet de changer deux item d'un tableau
	-- en changeant les indices
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

	-- procedure de tri a séléction utilisée pour détérminé la meilleure case 
	-- pour gagner ou bloquer le joueur 
	procedure sort_array(count : integer; t : in out T_array_all; mode : integer := 0) is
		ind 		: natural := 0;
		tmp_mine 	: integer := -10;
		tmp_his 	: integer := -10; 
	begin
		if count < t'last(1) then
			for i in count .. t'last(1) loop
				for j in t'range(2) loop
					for y in 2 .. 5 loop
						if t(i,j,y) > tmp_his then 
							tmp_his := t(i,j,y); -- on prend la plus grande de valeur (jetons alignés / bloqués totaux)
							if mode = 0 then
								ind := i; -- on garde son indice
							end if;
						end if;	
					end loop;
					if mode /= 0 then
						for y in 1..5 loop
							if t(i,j,y) = tmp_his then
								if t(i,j,1) >= tmp_mine and j=0 then -- j=0 quand on regarde les jetons de l'IA
									tmp_mine := t(i,j,1); -- on regarde le plus de jeton alignées en une fois
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

	-- procedure utilisée pour le déboguage elle permetat d'afficher le tableau
	-- qui tri les meilleures possibilités pour l'IA
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

	-- fonction qui retourne la meilleure case pour l'IA elle utilise toutes les autres fonctions procedures de l'IA
	function choose_case(t : T_board; a_available : T_available; state : Etat) return integer is
		result : integer := -1;
		a_ia : T_array_ia;
		a_all : T_array_all;
		b_loop : boolean := true;
	begin
		for i in a_available'range loop -- pour chaque case possible
			if (a_available(i) > -1) then
				a_ia := check_neigbours(a_available(i), i, t, state); -- on vérifie ses voisins 
				for j in a_ia'range(1) loop
					for k in a_ia'range(2) loop
						a_all(i,j,k) := a_ia(j,k); -- on stoque les informations dans un tableau
					end loop;	
				end loop;
			else
				for k in a_ia'range(2) loop -- si une colonne est indisponible on met le tableau a zéro comme ca il influencera pas le tri
					a_all(i,0,k) := 0;	
					a_all(i,1,k) := 0;	
				end loop;
			end if;
		end loop;

		sort_array(0, a_all); -- on trie le tableau par le nombre de jetons bloquées / alignées

		sort_array(0, a_all,1); -- on retrie le tableau par le plus de jeton bloqués / triés en une ligne

		for i in a_all'range(1) loop
			for k in 2 .. 5 loop
			if b_loop then

				if (a_all(i,0,k) = 3) then -- on vérifie une derniere fois si on a la possibilité de gagner
					result := a_all(i,0,0);
					b_loop := false;

				elsif (a_all(i,1,k) = 3) then -- on vérifie une derniere fois si on a la possibilité d'empecher le joueur de gagner
					result := a_all(i,1,0);
					b_loop := false;

				else
					result := a_all(0,1,0); -- sinon on prend la meilleure case possible
				end if;
			end if;	
			end loop;	
		end loop;

		return result; -- on retourne la colonne que l'IA va utiliser
	end;

-- Variables globales

	Board  		: T_board(T_indice_line,T_indice_colonne) := (others=>(others=>free));
	state  		: Etat;
	b_win  		: boolean := false;
	nb_win 		: positive := 4;
	error  		: integer := 0;
	b_answer 	: boolean := false;
	nb_players	: natural := 0;
	col 		: integer := -1;
	tmp 		: integer;

begin -- corps de la procédure principale
	state := cross;

	while not b_answer loop -- Tant que le joueur n'as pas donné une réponse accéptable
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

	for i in 0 .. 41 loop -- la boucle de jeu si on dépasse 42 tours c'est l'égalité 42 = 6 * 7 = colonnes * lignes
		if(not b_win) then
	   		Put("au tour de ");Put(state);new_line;

			if (nb_players = 2) or (nb_players = 1 and state = cross) then 	-- si on joue a deux ou si on joue a un et que l'etat = X alors on demande 
				col := -1;													-- a l'utilisateur de rentrer une case

		   		while col = -1 loop -- boucle qui vérifie que la colonne entrée sois valide

		   			if error > -1 then
						Put("Veuillez entrer la colonne dans laquelle vous voulez insérer votre jeton [0-6]: ");
					end if;

					Get(col);

					if(col >= 0) and (col <= 6) then
						if(check_col(Board, col)) then -- si tout est valide
							add_jeton(Board, state, col, nb_win, b_win);

							if( not b_win) then -- on change l'Etat seulement si la partie continue
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
						exit;
					end if;
		   		end loop;
			else
				tmp := choose_case(Board, check_available_case(Board), circle); -- utilisation d'une variable temporaire
				add_jeton(Board, state, tmp, nb_win, b_win);					-- car impossile de passer la fonction en paramètre

				if not b_win then
					state := change_state(state);
				end if;
		   	end if;
	   	end if;	
	end loop;

   	if b_win then
		Put(state);Put(" Gagné");new_line;new_line;new_line; -- on affiche qui a gagné 
	else
		Put(" Egalité"); -- si personne a gagné
	end if;
end puissance_4_gui;