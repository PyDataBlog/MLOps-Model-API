	DROP database IF EXISTS meal_planner;
	CREATE database meal_planner;
	USE meal_planner;
	
	CREATE TABLE user (
		user_id  int(10) NOT NULL AUTO_INCREMENT, 
		PRIMARY KEY ( user_id )
	);
     CREATE TABLE  account  ( 
        account_id int(10) NOT NULL, 
        preferred_calories  int(5) NOT NULL, 
        first_name  varchar(20) NOT NULL, 
        last_name  varchar(20) NOT NULL, 
        email  varchar(100) NOT NULL, 
        password_hash  varchar(30), 
       PRIMARY KEY ( email ) 
     );

     CREATE TABLE  food_item  ( 
        item_id  int(10) NOT NULL AUTO_INCREMENT, 
        calories  varchar(20) NOT NULL, 
        item_name  varchar(100) NOT NULL, 
        in_kitchen  varchar(5) DEFAULT 'Y', 
       PRIMARY KEY ( item_id ) 
     );
	 
     CREATE TABLE  meal_plan  ( 
        plan_id  int(10) NOT NULL AUTO_INCREMENT, 
        plan_name  varchar(20) NOT NULL, 
        no_of_weeks  int(5) NOT NULL, 
       PRIMARY KEY ( plan_id ) 
     );
	 
     CREATE TABLE  meal  ( 
        meal_id  int(10) NOT NULL AUTO_INCREMENT, 
        meal_name  varchar(20) NOT NULL, 
        calorie_count  int(5) NOT NULL, 
        meal_type  varchar(16) NOT NULL, 
       PRIMARY KEY ( meal_id ) 
     );
	 
     CREATE TABLE  shopping_list  ( 
        list_id  int(10) NOT NULL AUTO_INCREMENT, 
        total_cost  decimal(10,2) NOT NULL, 
       PRIMARY KEY ( list_id ) 
     );

     CREATE TABLE  meal_creation  ( 
      meal_id  int(10) NOT NULL, 
      user_id  int(10) NOT NULL, 
      creation_date  date NOT NULL, 
      PRIMARY KEY ( meal_id , user_id ), 
      FOREIGN KEY ( user_id )  
      REFERENCES  user  ( user_id ) ON DELETE CASCADE, 
      FOREIGN KEY ( meal_id )  
      REFERENCES  meal  ( meal_id ) ON DELETE CASCADE 
     );
	 
     CREATE TABLE  recipe  ( 
        recipe_id  int(10) NOT NULL AUTO_INCREMENT, 
        step_number  int(10), 
       PRIMARY KEY ( recipe_id ) 
     );
	 
     CREATE TABLE  image  (
        image_name  varchar(20) NOT NULL, 
       PRIMARY KEY ( image_name ) 
     );
	 
     CREATE TABLE  create_account  ( 
       user_id  int(10) NOT NULL, 
       email  varchar(20) NOT NULL, 
       PRIMARY KEY ( email ), 
       FOREIGN KEY ( email ) 
       REFERENCES  account  ( email ) ON DELETE CASCADE 
     );
	 
	 CREATE TABLE create_plan(
		plan_id int(10) NOT NULL,
		user_id int(10) NOT NULL,
		creation_date date NOT NULL,
		PRIMARY KEY (plan_id,user_id)
	 );

     CREATE TABLE  days  ( 
        week_number  int(5) NOT NULL, 
        day_name  varchar(10) NOT NULL, 
        breakfast  int(10) NOT NULL, 
        lunch  int(10) NOT NULL, 
        dinner  int(10) NOT NULL, 
        snack  int(10) NOT NULL, 
        step  varchar(50), 
       PRIMARY KEY ( day_name ), 
       FOREIGN KEY ( breakfast ) 
       REFERENCES  meal_creation  ( meal_id ) ON DELETE CASCADE, 
       FOREIGN KEY ( lunch ) 
          REFERENCES  meal_creation  ( meal_id ) ON DELETE CASCADE, 
       FOREIGN KEY ( dinner ) 
          REFERENCES  meal_creation  ( meal_id ) ON DELETE CASCADE, 
       FOREIGN KEY ( snack ) 
          REFERENCES  meal_creation  ( meal_id ) ON DELETE CASCADE 
     );
     CREATE TABLE  step  ( 
        recipe_id  int(10) NOT NULL, 
        step_id  int(10) NOT NULL, 
        step  varchar(50) NOT NULL, 
       PRIMARY KEY ( recipe_id ), 
       FOREIGN KEY ( recipe_id ) 
       REFERENCES  recipe  ( recipe_id ) ON DELETE CASCADE 
     );
	 
	 CREATE TABLE recipes_created(
		email varchar(20) NOT NULL,
		recipie_id int(10) NOT NULL,
		PRIMARY KEY (email)
	 );

     CREATE TABLE  item  ( 
        item_id  int(10) NOT NULL, 
        quantity  int(10) NOT NULL, 
        price  decimal(10,2) NOT NULL, 
       PRIMARY KEY ( item_id ) 
     );
	 
	 CREATE TABLE upload_image (
		image_name varchar(20),
		meal_id int(10),
		PRIMARY KEY (meal_id)
	 );

     CREATE TABLE  ingredients  ( 
        recipe_id  int(10) NOT NULL, 
        item_id  int(10) NOT NULL, 
        measurements  int(10) default 3, 
       PRIMARY KEY ( recipe_id , item_id ) 
     );
	

/*--User--*/

insert into user(user_id) values(
3300000);
insert into user(user_id) values(
3300001);
insert into user(user_id) values(
3300002);
insert into user(user_id) values(
3300003);
insert into user(user_id) values(
3300004);


/*--Account--*/

insert into account(account_id, preferred_calories, first_name, last_name, email,password_hash) values
(3300000, 25005, 'ida', 'Channa', 'ida.Channa@university.com', '8b9v2w7e5u5u5l0k4c9k4r6p6f3b');
insert into account(account_id, preferred_calories, first_name, last_name, email,password_hash) values
(3300001, 25006, 'pattin', 'Bass', 'pattin.Bass@icloud.com', '0i5b7l9l9q6h5t0v6m6e6h0c4r8u');
/*-- Food_Item--*/
insert into food_item(item_id, calories, item_name) values
(888000, 444000, 'Acetaldehyde _ Ethanal');
insert into food_item(item_id, calories, item_name) values
(888001, 444001, 'Acetic Acid');
insert into food_item(item_id, calories, item_name) values
(888002, 444002, 'Acetic Anhydride _ Acetyl Oxide; Acetic Oxide');
insert into food_item(item_id, calories, item_name) values
(888003, 444003, 'Acetoin _ Acetyl Methyl Carbinol');
insert into food_item(item_id, calories, item_name) values
(888004, 444004, 'Acetylated Sucrose Distearte');
insert into food_item(item_id, calories, item_name) values
(888005, 444005, 'Acetylmethylcarbinol');
insert into food_item(item_id, calories, item_name) values
(888006, 444006, 'Alanine');
insert into food_item(item_id, calories, item_name) values
(888007, 444007, 'Alcloxa _ Aluminum Chlorohydroxy Allantoinate');
insert into food_item(item_id, calories, item_name) values
(888008, 444008, 'Aldol');
insert into food_item(item_id, calories, item_name) values
(888009, 444009, 'Allantoin');
insert into food_item(item_id, calories, item_name) values
(888010, 444010, 'Allantoin Acetyl Methionine');
insert into food_item(item_id, calories, item_name) values
(888011, 444011, 'Allantoin Ascorbate');
insert into food_item(item_id, calories, item_name) values
(888012, 444012, 'Allantoin Biotin');
insert into food_item(item_id, calories, item_name) values
(888013, 444013, 'Allantoin Calcium Pantothenate');
insert into food_item(item_id, calories, item_name) values
(888014, 444014, 'Allantoin Galacturonic Acid');
insert into food_item(item_id, calories, item_name) values
(888015, 444015, 'Allantoin Glycyrrhetinic Acid');
insert into food_item(item_id, calories, item_name) values
(888016, 444016, 'Allantoin Polygalacturonic Acid');
insert into food_item(item_id, calories, item_name) values
(888017, 444017, 'Allantoinate');
insert into food_item(item_id, calories, item_name) values
(888018, 444018, "Aluminum Acetate _ Burow's Solution");
insert into food_item(item_id, calories, item_name) values
(888019, 444019, 'Aluminum Chorhydroxy Allantoinate');
insert into food_item(item_id, calories, item_name) values
(888020, 444020, 'Aluminum Distearate');
insert into food_item(item_id, calories, item_name) values
(888021, 444021, 'Aluminum Isostearates/Laurates/Stearates');
insert into food_item(item_id, calories, item_name) values
(888022, 444022, 'Aluminum Isostearates/Myristates');
insert into food_item(item_id, calories, item_name) values
(888023, 444023, 'Aluminum Isostearates/Palmitates');
insert into food_item(item_id, calories, item_name) values
(888024, 444024, 'Aluminum Lactate');
insert into food_item(item_id, calories, item_name) values
(888025, 444025, 'Aluminum Myristates/Palmitates');
insert into food_item(item_id, calories, item_name) values
(888026, 444026, 'Aluminum Salts (Aluminum Acetate, Alumi  num Lanolate, Aluminum');
insert into food_item(item_id, calories, item_name) values
(888027, 444027, 'Stearate,');
insert into food_item(item_id, calories, item_name) values
(888028, 444028, 'Aluminum Tristearate)');
insert into food_item(item_id, calories, item_name) values
(888029, 444029, 'Aluminum Stearates');
insert into food_item(item_id, calories, item_name) values
(888030, 444030, 'Aluminum Tripalmitate/Triisostearate');
insert into food_item(item_id, calories, item_name) values
(888031, 444031, 'Aluminum Tristearate');
insert into food_item(item_id, calories, item_name) values
(888032, 444032, 'Ammonium C12-15 Pareth Sulfate _ Pareth-25-3 Sulfate');
insert into food_item(item_id, calories, item_name) values
(888033, 444033, 'Ammonium Isostearate');
insert into food_item(item_id, calories, item_name) values
(888034, 444034, 'Ammonium Myristyl Sulfate');
insert into food_item(item_id, calories, item_name) values
(888035, 444035, 'Ammonium Oleate');
insert into food_item(item_id, calories, item_name) values
(888036, 444036, 'Ammonium Stearate _ Stearic Acid; Ammonium Salt');
insert into food_item(item_id, calories, item_name) values
(888037, 444037, 'Amphoteric');
insert into food_item(item_id, calories, item_name) values
(888038, 444038, 'Amphoteric-2');
insert into food_item(item_id, calories, item_name) values
(888039, 444039, 'Ascorbyl Stearate');
insert into food_item(item_id, calories, item_name) values
(888040, 444040, 'Asparagine');
insert into food_item(item_id, calories, item_name) values
(888041, 444041, 'Aspartic-Acid _ DL & L Forms; Aminosuccinate Acid');
insert into food_item(item_id, calories, item_name) values
(888042, 444042, '');
insert into food_item(item_id, calories, item_name) values
(888043, 444043, 'B');
insert into food_item(item_id, calories, item_name) values
(888044, 444044, 'Basic Voilet 10');
insert into food_item(item_id, calories, item_name) values
(888045, 444045, 'Beheneth-5, -10, -20, -30');
insert into food_item(item_id, calories, item_name) values
(888046, 444046, 'Behenic Acid _ Docosanoic Acid');
insert into food_item(item_id, calories, item_name) values
(888047, 444047, 'Behenic Acid _ Docosanol');
insert into food_item(item_id, calories, item_name) values
(888048, 444048, 'Beta-Carotene _ Provitamin A; BetaCarotene');
insert into food_item(item_id, calories, item_name) values
(888049, 444049, 'Betaine');
insert into food_item(item_id, calories, item_name) values
(888050, 444050, 'Biotin _ Vitamin H; Vitamin B Factor');
insert into food_item(item_id, calories, item_name) values
(888051, 444051, 'Brilliantines');
insert into food_item(item_id, calories, item_name) values
(888052, 444052, "Burow's Solution");
insert into food_item(item_id, calories, item_name) values
(888053, 444053, 'Butyl Acetate _ Acetic Acid; Butyl Ester');
insert into food_item(item_id, calories, item_name) values
(888054, 444054, 'Butyl Glycolate');
insert into food_item(item_id, calories, item_name) values
(888055, 444055, 'Butyl Oleate');
insert into food_item(item_id, calories, item_name) values
(888056, 444056, 'Butyl Palmitate');
insert into food_item(item_id, calories, item_name) values
(888057, 444057, 'Butyl Phrhaly Butyl Glycolate');
insert into food_item(item_id, calories, item_name) values
(888058, 444058, 'Butylrolactone _ Butanolide');
insert into food_item(item_id, calories, item_name) values
(888059, 444059, '');
insert into food_item(item_id, calories, item_name) values
(888060, 444060, 'C');
insert into food_item(item_id, calories, item_name) values
(888061, 444061, 'C18-36 Acid');
insert into food_item(item_id, calories, item_name) values
(888062, 444062, 'C29-70 Acid _ C29-70 Carboxylic Acids');
insert into food_item(item_id, calories, item_name) values
(888063, 444063, 'C18-36 Acid Glycol Ester');
insert into food_item(item_id, calories, item_name) values
(888064, 444064, 'C18-36 Acid Triglyceride ');
insert into food_item(item_id, calories, item_name) values
(888065, 444065, 'C9-11 Alcohols');
insert into food_item(item_id, calories, item_name) values
(888066, 444066, 'C12-16 Alcohols');
insert into food_item(item_id, calories, item_name) values
(888067, 444067, 'C14-15 Alcohols');
insert into food_item(item_id, calories, item_name) values
(888068, 444068, 'C12-15 Alcohols Benzoate');
insert into food_item(item_id, calories, item_name) values
(888069, 444069, 'C12-15 Alcohols Lactate');
insert into food_item(item_id, calories, item_name) values
(888070, 444070, 'C21 Dicarboxylic Acid');
insert into food_item(item_id, calories, item_name) values
(888071, 444071, 'C15-18 Glycol');
insert into food_item(item_id, calories, item_name) values
(888072, 444072, 'C18-20 Glycol Palmitate');
insert into food_item(item_id, calories, item_name) values
(888073, 444073, 'C8-9, C9-11, C9-13, C9-14, C10-11, C10-  13, C11-12, C11-13,');
insert into food_item(item_id, calories, item_name) values
(888074, 444074, 'C12-14, C13-14, C13-16, and C20-40 ');
insert into food_item(item_id, calories, item_name) values
(888075, 444075, 'IsoParaffins');
insert into food_item(item_id, calories, item_name) values
(888076, 444076, 'C11-15 Pareth-12 Stearate');
insert into food_item(item_id, calories, item_name) values
(888077, 444077, 'C11-15 Pareth-40');
insert into food_item(item_id, calories, item_name) values
(888078, 444078, 'C12-13 Pareth 3-7');
insert into food_item(item_id, calories, item_name) values
(888079, 444079, 'C14-15 Pareth-7, -11, -13');
insert into food_item(item_id, calories, item_name) values
(888080, 444080, 'C10-18 Triglycerieds ');
insert into food_item(item_id, calories, item_name) values
(888081, 444081, 'Calcium Stearate');
insert into food_item(item_id, calories, item_name) values
(888082, 444082, 'Calcium Stearoyl Lactylate');
insert into food_item(item_id, calories, item_name) values
(888083, 444083, 'Caproamphoacetate');
insert into food_item(item_id, calories, item_name) values
(888084, 444084, 'Caproamhodiacetate');
insert into food_item(item_id, calories, item_name) values
(888085, 444085, 'Capryl Betaine');
insert into food_item(item_id, calories, item_name) values
(888086, 444086, 'Caprylamine Oxide');
insert into food_item(item_id, calories, item_name) values
(888087, 444087, 'Caprylic / Capric / Stearic Triglyceride');
insert into food_item(item_id, calories, item_name) values
(888088, 444088, 'Caprylic Acid');
insert into food_item(item_id, calories, item_name) values
(888089, 444089, 'Caprylamphoacetate');
insert into food_item(item_id, calories, item_name) values
(888090, 444090, 'Capryloamphodiacetate');
insert into food_item(item_id, calories, item_name) values
(888091, 444091, 'Carbamide');
insert into food_item(item_id, calories, item_name) values
(888092, 444092, 'Cetearalkonium Bromide');
insert into food_item(item_id, calories, item_name) values
(888093, 444093, 'Ceteareth-3 _ Cetyl/Stearyl Ether');
insert into food_item(item_id, calories, item_name) values
(888094, 444094, 'Ceteareth-4, -6, -8, -10, -12, -15, -17, -20, -27, -30');
insert into food_item(item_id, calories, item_name) values
(888095, 444095, 'Ceteareth-5');
insert into food_item(item_id, calories, item_name) values
(888096, 444096, 'Cetaryl Alcohol');
insert into food_item(item_id, calories, item_name) values
(888097, 444097, 'Ceteth-1');
insert into food_item(item_id, calories, item_name) values
(888098, 444098, 'Cetyl-');
insert into food_item(item_id, calories, item_name) values
(888099, 444099, 'Cetyl Alcohol');
insert into food_item(item_id, calories, item_name) values
(888100, 444100, 'Cetyl Ammonium');
insert into food_item(item_id, calories, item_name) values
(888101, 444101, 'Cetyl Arachidate');
insert into food_item(item_id, calories, item_name) values
(888102, 444102, 'Cetyl Betaine');
insert into food_item(item_id, calories, item_name) values
(888103, 444103, 'Cetyl Esters');
insert into food_item(item_id, calories, item_name) values
(888104, 444104, 'Cetyl Lactate');
insert into food_item(item_id, calories, item_name) values
(888105, 444105, 'Cetyl Myristate');
insert into food_item(item_id, calories, item_name) values
(888106, 444106, 'Cetyl Octanoate');
insert into food_item(item_id, calories, item_name) values
(888107, 444107, 'Cetyl Palmitate');
insert into food_item(item_id, calories, item_name) values
(888108, 444108, 'Cetyl Phosphate');
insert into food_item(item_id, calories, item_name) values
(888109, 444109, 'Cetyl Ricinoleate');
insert into food_item(item_id, calories, item_name) values
(888110, 444110, 'Cetyl Stearate');
insert into food_item(item_id, calories, item_name) values
(888111, 444111, 'Cetyl Stearyl Glycol');
insert into food_item(item_id, calories, item_name) values
(888112, 444112, 'Cetylarachidol');
insert into food_item(item_id, calories, item_name) values
(888113, 444113, 'Cetylpyridinium Chloride');
insert into food_item(item_id, calories, item_name) values
(888114, 444114, 'Cetyltrymethylammonium BromideChitin');
insert into food_item(item_id, calories, item_name) values
(888115, 444115, 'Cloflucarbon');
insert into food_item(item_id, calories, item_name) values
(888116, 444116, 'Deceth-7-Carboxylic Acid');
insert into food_item(item_id, calories, item_name) values
(888117, 444117, 'Decyl Betaine');
insert into food_item(item_id, calories, item_name) values
(888118, 444118, 'Diacetyl');
insert into food_item(item_id, calories, item_name) values
(888119, 444119, 'Diazo-');
insert into food_item(item_id, calories, item_name) values
(888120, 444120, 'Diazolidinyl Urea _ Germall II (TM)');
insert into food_item(item_id, calories, item_name) values
(888121, 444121, 'Dicetyl Adipate');
insert into food_item(item_id, calories, item_name) values
(888122, 444122, 'Dicetyl Thiodipropionate');
insert into food_item(item_id, calories, item_name) values
(888123, 444123, 'Diethyl Asparate');
insert into food_item(item_id, calories, item_name) values
(888124, 444124, 'Diethyl Palmitoyl Apartate');
insert into food_item(item_id, calories, item_name) values
(888125, 444125, 'Diethyl Sebacate');
insert into food_item(item_id, calories, item_name) values
(888126, 444126, 'Diethylaminoethyl Stearamide');
insert into food_item(item_id, calories, item_name) values
(888127, 444127, 'Diethylaminoethyl Stearate');
insert into food_item(item_id, calories, item_name) values
(888128, 444128, 'Diglyceryl Stearate Malate');
insert into food_item(item_id, calories, item_name) values
(888129, 444129, 'Dihydroxyethyl Soyamine Dioleate');
insert into food_item(item_id, calories, item_name) values
(888130, 444130, 'Dihydroxyethyl Stearamine Oxide');
insert into food_item(item_id, calories, item_name) values
(888131, 444131, 'Dihydroxyethyl Stearyl Glycinate');
insert into food_item(item_id, calories, item_name) values
(888132, 444132, 'Dimethyl Behenamine');
insert into food_item(item_id, calories, item_name) values
(888133, 444133, 'Dimethyl Lauramine Oleate');
insert into food_item(item_id, calories, item_name) values
(888134, 444134, 'Dimethyl Myristamine');
insert into food_item(item_id, calories, item_name) values
(888135, 444135, 'Dimethyl Palmitamine');
insert into food_item(item_id, calories, item_name) values
(888136, 444136, 'Dimethyl Stearamine');
insert into food_item(item_id, calories, item_name) values
(888137, 444137, 'Dimethylaminopropyl Oleamide');
insert into food_item(item_id, calories, item_name) values
(888138, 444138, 'Dimethylaminopropyl Stearamide');
insert into food_item(item_id, calories, item_name) values
(888139, 444139, 'Dimethylol Urea');
insert into food_item(item_id, calories, item_name) values
(888140, 444140, 'Dimyristyl Thiodipropionate');
insert into food_item(item_id, calories, item_name) values
(888141, 444141, 'Dioleth-8-Phosphate');
insert into food_item(item_id, calories, item_name) values
(888142, 444142, 'Direct Black 51');
insert into food_item(item_id, calories, item_name) values
(888143, 444143, 'Direct Red 23 _ Fast Scarlet 4BSA');
insert into food_item(item_id, calories, item_name) values
(888144, 444144, 'Direct Red 80');
insert into food_item(item_id, calories, item_name) values
(888145, 444145, 'Direct Violet 48');
insert into food_item(item_id, calories, item_name) values
(888146, 444146, 'Direct Yellow 12 _ Chrysophenine G');
insert into food_item(item_id, calories, item_name) values
(888147, 444147, 'Disodium Cetaeryl Sulfosuccinate');
insert into food_item(item_id, calories, item_name) values
(888148, 444148, 'Disodium Isostearamino Mea- Sulfosuccinate');
insert into food_item(item_id, calories, item_name) values
(888149, 444149, 'Disodium Monooleamidosulfosuccinate');
insert into food_item(item_id, calories, item_name) values
(888150, 444150, 'Disodium Monoricinoleamido Mea-  Sulfosuccinate');
insert into food_item(item_id, calories, item_name) values
(888151, 444151, 'Disodium Oleamido MIPA-Sulfosuccinate');
insert into food_item(item_id, calories, item_name) values
(888152, 444152, 'Disodium Oleamido PEG-2 Sulfosuccinate');
insert into food_item(item_id, calories, item_name) values
(888153, 444153, 'Disodium Oleyl Sulfosuccinate');
insert into food_item(item_id, calories, item_name) values
(888154, 444154, 'Disodium Stearmido MEA-Sulfosuccinate');
insert into food_item(item_id, calories, item_name) values
(888155, 444155, 'Disodium Stearminodipionate');
insert into food_item(item_id, calories, item_name) values
(888156, 444156, 'Disodium Stearyl Sulfosuccinate');
insert into food_item(item_id, calories, item_name) values
(888157, 444157, 'Distearyl Thiodipropionate');
insert into food_item(item_id, calories, item_name) values
(888158, 444158, 'DI-TEA-Palmitoyl Asparate');
insert into food_item(item_id, calories, item_name) values
(888159, 444159, 'Dodecanedionic Acid; Cetearyl Alcohol; Glycol Copolymer');
insert into food_item(item_id, calories, item_name) values
(888160, 444160, 'Dodecyltetradecanol');
insert into food_item(item_id, calories, item_name) values
(888161, 444161, 'Enfleurage');
insert into food_item(item_id, calories, item_name) values
(888162, 444162, 'Enzyme');
insert into food_item(item_id, calories, item_name) values
(888163, 444163, 'Ethyl Aspartate');
insert into food_item(item_id, calories, item_name) values
(888164, 444164, 'Ethyl Oleate');
insert into food_item(item_id, calories, item_name) values
(888165, 444165, 'Ethyl Palmitate');
insert into food_item(item_id, calories, item_name) values
(888166, 444166, 'Ethyl Serinate');
insert into food_item(item_id, calories, item_name) values
(888167, 444167, 'Ethyl Stearate');
insert into food_item(item_id, calories, item_name) values
(888168, 444168, 'Ethyl Urocanate');
insert into food_item(item_id, calories, item_name) values
(888169, 444169, 'Ethylene Dioleamide');
insert into food_item(item_id, calories, item_name) values
(888170, 444170, 'Ethylene Distearamide');
insert into food_item(item_id, calories, item_name) values
(888171, 444171, 'Ethylene Urea');
insert into food_item(item_id, calories, item_name) values
(888172, 444172, 'Ethylhexyl Palmitate');
insert into food_item(item_id, calories, item_name) values
(888173, 444173, 'Fatty Alcohols _ Cetyl; Stearyl; Lauryl;  Myristyl');
insert into food_item(item_id, calories, item_name) values
(888174, 444174, 'Folic Acid');
insert into food_item(item_id, calories, item_name) values
(888175, 444175, 'Fructose');
insert into food_item(item_id, calories, item_name) values
(888176, 444176, 'Gel (not Silica gel)');
insert into food_item(item_id, calories, item_name) values
(888177, 444177, 'Glucose Glutamate');
insert into food_item(item_id, calories, item_name) values
(888178, 444178, 'Glyceryl Caprate');
insert into food_item(item_id, calories, item_name) values
(888179, 444179, 'Glyceryl Caprylate');
insert into food_item(item_id, calories, item_name) values
(888180, 444180, 'Glyceryl Caprylate/Caprate');
insert into food_item(item_id, calories, item_name) values
(888181, 444181, 'Glyceryl Dioleate');
insert into food_item(item_id, calories, item_name) values
(888182, 444182, 'Glyceryl Distearate');
insert into food_item(item_id, calories, item_name) values
(888183, 444183, 'Glyceryl Hydrostearate');
insert into food_item(item_id, calories, item_name) values
(888184, 444184, 'Glyceryl Hydrostearate');
insert into food_item(item_id, calories, item_name) values
(888185, 444185, 'Glyceryl Hydroxystearate');
insert into food_item(item_id, calories, item_name) values
(888186, 444186, 'Glyceryl Isostearate');
insert into food_item(item_id, calories, item_name) values
(888187, 444187, 'Glyceryl Monostearate');
insert into food_item(item_id, calories, item_name) values
(888188, 444188, 'Glyceryl Myristate');
insert into food_item(item_id, calories, item_name) values
(888189, 444189, 'Glyceryl Oleate');
insert into food_item(item_id, calories, item_name) values
(888190, 444190, 'Glyceryl Palmitate Lactate');
insert into food_item(item_id, calories, item_name) values
(888191, 444191, 'Glyceryl Stearate SE');
insert into food_item(item_id, calories, item_name) values
(888192, 444192, 'Glyceryl Trimyristate');
insert into food_item(item_id, calories, item_name) values
(888193, 444193, 'Glycol Stearate SE');
insert into food_item(item_id, calories, item_name) values
(888194, 444194, 'Glycyrrhetinyl Stearate');
insert into food_item(item_id, calories, item_name) values
(888195, 444195, 'Guanidine Carbonate');
insert into food_item(item_id, calories, item_name) values
(888196, 444196, 'Guanosine');
insert into food_item(item_id, calories, item_name) values
(888197, 444197, 'Hexanediol Distearate');
insert into food_item(item_id, calories, item_name) values
(888198, 444198, 'Histidine');
insert into food_item(item_id, calories, item_name) values
(888199, 444199, 'Hydrogenated Fatty Oils');
insert into food_item(item_id, calories, item_name) values
(888200, 444200, 'Hydroxylated Lecithin');
insert into food_item(item_id, calories, item_name) values
(888201, 444201, 'Hydroxyoctacosanyl Hydroxyastearate');
insert into food_item(item_id, calories, item_name) values
(888202, 444202, 'Hydroxystearmide MEA');
insert into food_item(item_id, calories, item_name) values
(888203, 444203, 'Hydroxystearic Acid');
insert into food_item(item_id, calories, item_name) values
(888204, 444204, 'Imidazlidinyl Urea');
insert into food_item(item_id, calories, item_name) values
(888205, 444205, 'Indole');
insert into food_item(item_id, calories, item_name) values
(888206, 444206, 'Isobutyl Myristate');
insert into food_item(item_id, calories, item_name) values
(888207, 444207, 'Isobutyl Palmitate');
insert into food_item(item_id, calories, item_name) values
(888208, 444208, 'Isobutyl Stearate');
insert into food_item(item_id, calories, item_name) values
(888209, 444209, 'Isoceteth-10, -20, -30');
insert into food_item(item_id, calories, item_name) values
(888210, 444210, 'Isocetyl Alcohol');
insert into food_item(item_id, calories, item_name) values
(888211, 444211, 'Isocetyl Isodecanoate');
insert into food_item(item_id, calories, item_name) values
(888212, 444212, 'Isocetyl Palmitate');
insert into food_item(item_id, calories, item_name) values
(888213, 444213, 'Isocetyl Stearate');
insert into food_item(item_id, calories, item_name) values
(888214, 444214, 'Isocetyl Stearoyl Stearate');
insert into food_item(item_id, calories, item_name) values
(888215, 444215, 'Isoceteth-10 Stearate');
insert into food_item(item_id, calories, item_name) values
(888216, 444216, 'Isodecyl hydroxystearate');
insert into food_item(item_id, calories, item_name) values
(888217, 444217, 'Isodecyl Myristate');
insert into food_item(item_id, calories, item_name) values
(888218, 444218, 'Isodecyl Oleate');
insert into food_item(item_id, calories, item_name) values
(888219, 444219, 'Isodecyl Palmitate');
insert into food_item(item_id, calories, item_name) values
(888220, 444220, 'Isohyxyl Palmitate');
insert into food_item(item_id, calories, item_name) values
(888221, 444221, 'Isopropyl Acetate');
insert into food_item(item_id, calories, item_name) values
(888222, 444222, 'Isopropyl Isostearate');
insert into food_item(item_id, calories, item_name) values
(888223, 444223, 'Isopropyl Myristate');
insert into food_item(item_id, calories, item_name) values
(888224, 444224, 'Isopropyl Palmitate');
insert into food_item(item_id, calories, item_name) values
(888225, 444225, 'Isopropyl Stearate');
insert into food_item(item_id, calories, item_name) values
(888226, 444226, 'Isostearamidopropalkonium Chloride');
insert into food_item(item_id, calories, item_name) values
(888227, 444227, 'Isostearamidopropyl Betaine');
insert into food_item(item_id, calories, item_name) values
(888228, 444228, 'Isostearamidopropyl');
insert into food_item(item_id, calories, item_name) values
(888229, 444229, 'Dimethylamine Glycolate');
insert into food_item(item_id, calories, item_name) values
(888230, 444230, 'Isostearamidopropyl Dimethylamine Lactate');
insert into food_item(item_id, calories, item_name) values
(888231, 444231, 'Isostearamidopropyl EthyldimoniumEthosulfate');
insert into food_item(item_id, calories, item_name) values
(888232, 444232, 'Isostearamidopropyl Morpholine Lactate');
insert into food_item(item_id, calories, item_name) values
(888233, 444233, 'Isostearamidoporopylamine Oxide');
insert into food_item(item_id, calories, item_name) values
(888234, 444234, 'Isosteareth-2 through -20');
insert into food_item(item_id, calories, item_name) values
(888235, 444235, 'Isostearic Acid');
insert into food_item(item_id, calories, item_name) values
(888236, 444236, 'Isostearoamphoglycinate');
insert into food_item(item_id, calories, item_name) values
(888237, 444237, 'Isostearoamphopropionate');
insert into food_item(item_id, calories, item_name) values
(888238, 444238, 'Isostearyl Alcohol');
insert into food_item(item_id, calories, item_name) values
(888239, 444239, 'Isostearyl Benzylimidonium Chloride');
insert into food_item(item_id, calories, item_name) values
(888240, 444240, 'Isostearyl Diglyceryl Succinate');
insert into food_item(item_id, calories, item_name) values
(888241, 444241, 'Isostearyl Erucate');
insert into food_item(item_id, calories, item_name) values
(888242, 444242, 'Isostearyl Ethylimidonium Ethosulfate');
insert into food_item(item_id, calories, item_name) values
(888243, 444243, 'Isostearyl Hydroxyethyl Imidazoline');
insert into food_item(item_id, calories, item_name) values
(888244, 444244, 'Isostearyl Imidazoline');
insert into food_item(item_id, calories, item_name) values
(888245, 444245, 'Isostearyl Isostearate');
insert into food_item(item_id, calories, item_name) values
(888246, 444246, 'Isostearyl Lactate');
insert into food_item(item_id, calories, item_name) values
(888247, 444247, 'Isostearyl Neopentanoate');
insert into food_item(item_id, calories, item_name) values
(888248, 444248, 'Isostearyl Palmitate');
insert into food_item(item_id, calories, item_name) values
(888249, 444249, 'Isostearyl Stearoyl STearate');
insert into food_item(item_id, calories, item_name) values
(888250, 444250, 'Lactic Acid');
insert into food_item(item_id, calories, item_name) values
(888251, 444251, 'Lauroyl Sarcosine');
insert into food_item(item_id, calories, item_name) values
(888252, 444252, 'Lauryl Isostearate');
insert into food_item(item_id, calories, item_name) values
(888253, 444253, 'Lauryl Palmitate');
insert into food_item(item_id, calories, item_name) values
(888254, 444254, 'Lauryl Stearate');
insert into food_item(item_id, calories, item_name) values
(888255, 444255, 'Lauryl Suntaine');
insert into food_item(item_id, calories, item_name) values
(888256, 444256, 'Lithium Stearate');
insert into food_item(item_id, calories, item_name) values
(888257, 444257, 'Magnesium Myristate Magnesium Oleate');
insert into food_item(item_id, calories, item_name) values
(888258, 444258, 'Magnesium Stearate');
insert into food_item(item_id, calories, item_name) values
(888259, 444259, 'Methyl Gluceth-10 or -20');
insert into food_item(item_id, calories, item_name) values
(888260, 444260, 'Methyl Glucet-20 Sesquistereate _Glucamate');
insert into food_item(item_id, calories, item_name) values
(888261, 444261, 'Methyl Glucose Sesquioleate');
insert into food_item(item_id, calories, item_name) values
(888262, 444262, 'Methyl Glucose Sesquistearate');
insert into food_item(item_id, calories, item_name) values
(888263, 444263, 'Methyl Hydroxystearate');
insert into food_item(item_id, calories, item_name) values
(888264, 444264, 'Methyl Lactate');
insert into food_item(item_id, calories, item_name) values
(888265, 444265, 'Methyl Myristate');
insert into food_item(item_id, calories, item_name) values
(888266, 444266, 'Methyl Oleate');
insert into food_item(item_id, calories, item_name) values
(888267, 444267, 'Methyl Palmitate');
insert into food_item(item_id, calories, item_name) values
(888268, 444268, 'Mixed Isopropanolamines');
insert into food_item(item_id, calories, item_name) values
(888269, 444269, 'Myristate');
insert into food_item(item_id, calories, item_name) values
(888270, 444270, 'Morpholine Stearate');
insert into food_item(item_id, calories, item_name) values
(888271, 444271, 'Myreth-3');
insert into food_item(item_id, calories, item_name) values
(888272, 444272, 'Myreth-3 Caprate _ Myristic EthoxyCaprate');
insert into food_item(item_id, calories, item_name) values
(888273, 444273, 'Myreth-3 Laurate');
insert into food_item(item_id, calories, item_name) values
(888274, 444274, 'Myreth-3 Myristate');
insert into food_item(item_id, calories, item_name) values
(888275, 444275, 'Myreth-4');
insert into food_item(item_id, calories, item_name) values
(888276, 444276, 'Myristamide DEA _ Myristic Diethanolamide');
insert into food_item(item_id, calories, item_name) values
(888277, 444277, 'Myristamide MIPA');
insert into food_item(item_id, calories, item_name) values
(888278, 444278, 'Myristamidopropyl Betaine');
insert into food_item(item_id, calories, item_name) values
(888279, 444279, 'Myristamidopropyl Diethylamine');
insert into food_item(item_id, calories, item_name) values
(888280, 444280, 'Myristamidopropylamine Oxide');
insert into food_item(item_id, calories, item_name) values
(888281, 444281, 'Myristamine Oxide');
insert into food_item(item_id, calories, item_name) values
(888282, 444282, 'Myristaminopropionic Acid');
insert into food_item(item_id, calories, item_name) values
(888283, 444283, 'Myristate');
insert into food_item(item_id, calories, item_name) values
(888284, 444284, 'Myristic Acid');
insert into food_item(item_id, calories, item_name) values
(888285, 444285, 'Myristimide MEA');
insert into food_item(item_id, calories, item_name) values
(888286, 444286, 'Myristoamphoacetate');
insert into food_item(item_id, calories, item_name) values
(888287, 444287, 'Myristoyl Sarcosine');
insert into food_item(item_id, calories, item_name) values
(888288, 444288, 'Myristyl Alcohol');
insert into food_item(item_id, calories, item_name) values
(888289, 444289, 'Myristyl Betaine');
insert into food_item(item_id, calories, item_name) values
(888290, 444290, 'Myristyl Hydroxyethyl Imidazoline');
insert into food_item(item_id, calories, item_name) values
(888291, 444291, 'Myristyl Isostearate');
insert into food_item(item_id, calories, item_name) values
(888292, 444292, 'Myristyl Lactate');
insert into food_item(item_id, calories, item_name) values
(888293, 444293, 'Myristyl Myristate');
insert into food_item(item_id, calories, item_name) values
(888294, 444294, 'Myristyl Neopentanoate _ Ceraphyl');
insert into food_item(item_id, calories, item_name) values
(888295, 444295, 'Myristyl Propionate');
insert into food_item(item_id, calories, item_name) values
(888296, 444296, 'Myristyl Stearate');
insert into food_item(item_id, calories, item_name) values
(888297, 444297, 'Myristyleicosanol');
insert into food_item(item_id, calories, item_name) values
(888298, 444298, 'Myristyleicosyl Stearate');
insert into food_item(item_id, calories, item_name) values
(888299, 444299, 'Myristyloctadecanol');
insert into food_item(item_id, calories, item_name) values
(888300, 444300, 'Nonyl Acetate');
insert into food_item(item_id, calories, item_name) values
(888301, 444301, 'Octododecanol-2 _ Octyl Dodecanol');
insert into food_item(item_id, calories, item_name) values
(888302, 444302, 'Octododeceth-20, -25');
insert into food_item(item_id, calories, item_name) values
(888303, 444303, 'Octododecyl Myristate');
insert into food_item(item_id, calories, item_name) values
(888304, 444304, 'Octoxyglyceryl Behenate');
insert into food_item(item_id, calories, item_name) values
(888305, 444305, 'Octyl Acetoxystearate');
insert into food_item(item_id, calories, item_name) values
(888306, 444306, 'Octyl Hydroxystearate');
insert into food_item(item_id, calories, item_name) values
(888307, 444307, 'Octyl Palmitate');
insert into food_item(item_id, calories, item_name) values
(888308, 444308, 'Octyl Stearate');
insert into food_item(item_id, calories, item_name) values
(888309, 444309, 'Octyldocecanol');
insert into food_item(item_id, calories, item_name) values
(888310, 444310, 'Octyldodecyl Stearate');
insert into food_item(item_id, calories, item_name) values
(888311, 444311, 'Octyldodecyl Stearoyl Stearate');
insert into food_item(item_id, calories, item_name) values
(888312, 444312, 'Oleamide _ Oleylamide');
insert into food_item(item_id, calories, item_name) values
(888313, 444313, 'Oleamide DEA _ Oleic Diethanolamide');
insert into food_item(item_id, calories, item_name) values
(888314, 444314, 'Oleamide MIPA');
insert into food_item(item_id, calories, item_name) values
(888315, 444315, 'Oleamine Oxide');
insert into food_item(item_id, calories, item_name) values
(888316, 444316, 'Oleic Acid');
insert into food_item(item_id, calories, item_name) values
(888317, 444317, 'Oleoyl Sarcosine');
insert into food_item(item_id, calories, item_name) values
(888318, 444318, 'Oleth-3 Phosphate');
insert into food_item(item_id, calories, item_name) values
(888319, 444319, 'Oleth 20');
insert into food_item(item_id, calories, item_name) values
(888320, 444320, 'Oleth-20 Phosphate');
insert into food_item(item_id, calories, item_name) values
(888321, 444321, 'Oleyl Betaine');
insert into food_item(item_id, calories, item_name) values
(888322, 444322, 'Oleyl Myristate');
insert into food_item(item_id, calories, item_name) values
(888323, 444323, 'Oleyl Oleate');
insert into food_item(item_id, calories, item_name) values
(888324, 444324, 'Oleyl Stearate');
insert into food_item(item_id, calories, item_name) values
(888325, 444325, 'Orotic Acid _ Pyrimidecarboxylic Acid');
insert into food_item(item_id, calories, item_name) values
(888326, 444326, 'Palmamamidopropyl Betaine');
insert into food_item(item_id, calories, item_name) values
(888327, 444327, 'Palmitamide DEA, MEA');
insert into food_item(item_id, calories, item_name) values
(888328, 444328, 'Palmitamidopropyl Betaine');
insert into food_item(item_id, calories, item_name) values
(888329, 444329, 'Palmitamindopropyl Diethylamine');
insert into food_item(item_id, calories, item_name) values
(888330, 444330, 'Palmitamine');
insert into food_item(item_id, calories, item_name) values
(888331, 444331, 'Palmitamine Oxide _ PalmitylDimethylamine Oxide');
insert into food_item(item_id, calories, item_name) values
(888332, 444332, 'Palmitate');
insert into food_item(item_id, calories, item_name) values
(888333, 444333, 'Palmitic Acid');
insert into food_item(item_id, calories, item_name) values
(888334, 444334, 'Panthenyl Ethyl Etheracetate');
insert into food_item(item_id, calories, item_name) values
(888335, 444335, 'Pareth-25- 12');
insert into food_item(item_id, calories, item_name) values
(888336, 444336, 'PEG-9 Caprylate');
insert into food_item(item_id, calories, item_name) values
(888337, 444337, 'PEG-8 Caprylate / Caprate');
insert into food_item(item_id, calories, item_name) values
(888338, 444338, 'PEG-6 Caprylic / Capric Glycerides');
insert into food_item(item_id, calories, item_name) values
(888339, 444339, 'PEG-6 to -150 Dioleate');
insert into food_item(item_id, calories, item_name) values
(888340, 444340, 'PEG-3 Dipalmitate');
insert into food_item(item_id, calories, item_name) values
(888341, 444341, 'PEG-2 through -175 Distearate');
insert into food_item(item_id, calories, item_name) values
(888342, 444342, 'PEG-5 through -120 Glyceryl Stearate');
insert into food_item(item_id, calories, item_name) values
(888343, 444343, 'PEG-25 Glyceryl Trioleate');
insert into food_item(item_id, calories, item_name) values
(888344, 444344, 'PEG-6 or -12 Isostearate');
insert into food_item(item_id, calories, item_name) values
(888345, 444345, 'PEG-20 Methyl Glucose Sesquistearate');
insert into food_item(item_id, calories, item_name) values
(888346, 444346, 'PEG-4 Octanoate');
insert into food_item(item_id, calories, item_name) values
(888347, 444347, 'PEG-2 through -9 Oleamide');
insert into food_item(item_id, calories, item_name) values
(888348, 444348, 'PEG-2 through -30 Oleamide');
insert into food_item(item_id, calories, item_name) values
(888349, 444349, 'PEG-12, -20, or -30 Oleate');
insert into food_item(item_id, calories, item_name) values
(888350, 444350, 'PEG-3 through -150 Oleate');
insert into food_item(item_id, calories, item_name) values
(888351, 444351, 'PEG-6 through -20 Palmitate');
insert into food_item(item_id, calories, item_name) values
(888352, 444352, 'PEG-25 through -125 Propylene Glycol Stearate');
insert into food_item(item_id, calories, item_name) values
(888353, 444353, 'PEG-8 Sesquioleate');
insert into food_item(item_id, calories, item_name) values
(888354, 444354, 'PEG-5 or -20 Sorbitan Isostearate');
insert into food_item(item_id, calories, item_name) values
(888355, 444355, 'PEG-3 or -6 Sorbitan Oleate');
insert into food_item(item_id, calories, item_name) values
(888356, 444356, 'PEG-80 Sorbitan Palmitate');
insert into food_item(item_id, calories, item_name) values
(888357, 444357, 'PEG-40 Sorbitan Peroleate');
insert into food_item(item_id, calories, item_name) values
(888358, 444358, 'PEG-3 or -40 Sorbitan Stearate');
insert into food_item(item_id, calories, item_name) values
(888359, 444359, 'PEG-30, -40, or -60 Sorbitan Tetraoleate');
insert into food_item(item_id, calories, item_name) values
(888360, 444360, 'PEG-60 Sorbitan Tetrastearate');
insert into food_item(item_id, calories, item_name) values
(888361, 444361, 'PEG-2 through -150 Stearate');
insert into food_item(item_id, calories, item_name) values
(888362, 444362, 'PEG-66 or -200 Tryhydroxystearin');
insert into food_item(item_id, calories, item_name) values
(888363, 444363, 'Pentaerythrityl Tetraoctanoate');
insert into food_item(item_id, calories, item_name) values
(888364, 444364, 'Pentaerythrityl Tetrastearate and  CalciumStearate');
insert into food_item(item_id, calories, item_name) values
(888365, 444365, 'Phospholipids _ Phosphatides');
insert into food_item(item_id, calories, item_name) values
(888366, 444366, 'Polyglycerol');
insert into food_item(item_id, calories, item_name) values
(888367, 444367, 'Polyglycerol-4 Cocoate');
insert into food_item(item_id, calories, item_name) values
(888368, 444368, 'Polyglycerol-10 Decalinoleate');
insert into food_item(item_id, calories, item_name) values
(888369, 444369, 'Polyglycerol-2 Diisostearate');
insert into food_item(item_id, calories, item_name) values
(888370, 444370, 'Polyglycerol-6 Dioleate');
insert into food_item(item_id, calories, item_name) values
(888371, 444371, 'Polyglycerol-6 Distearate');
insert into food_item(item_id, calories, item_name) values
(888372, 444372, 'Polyglycerol-3 Hydroxylauryl Ether');
insert into food_item(item_id, calories, item_name) values
(888373, 444373, 'Polyglycerol-4 Isostearate');
insert into food_item(item_id, calories, item_name) values
(888374, 444374, 'Polyglycerol-3, -4 or -8 Oleate');
insert into food_item(item_id, calories, item_name) values
(888375, 444375, 'Polyglycerol-2 or -4 Oleyl Ether');
insert into food_item(item_id, calories, item_name) values
(888376, 444376, 'Polyglycerol-2 PEG-4 Stearate');
insert into food_item(item_id, calories, item_name) values
(888377, 444377, 'Polyglycerol-2 Sesquiisostearate');
insert into food_item(item_id, calories, item_name) values
(888378, 444378, 'Polyglycerol-2 Sesquioleate');
insert into food_item(item_id, calories, item_name) values
(888379, 444379, 'Polyglycerol-3, -4 or -8 Stearate');
insert into food_item(item_id, calories, item_name) values
(888380, 444380, 'Polyglycerol-10 Tertraoleate');
insert into food_item(item_id, calories, item_name) values
(888381, 444381, 'Polyglycerol-2 Tetrastearate');
insert into food_item(item_id, calories, item_name) values
(888382, 444382, 'Polysorbate 60 and Polysorbate 80');
insert into food_item(item_id, calories, item_name) values
(888383, 444383, 'Potassium Apartate');
insert into food_item(item_id, calories, item_name) values
(888384, 444384, 'Potassium Coco-Hydrolyzed Protein');
insert into food_item(item_id, calories, item_name) values
(888385, 444385, 'Potassium DNA');
insert into food_item(item_id, calories, item_name) values
(888386, 444386, 'Potassium Oleate-Oleic Acid');
insert into food_item(item_id, calories, item_name) values
(888387, 444387, 'Potassium Salt');
insert into food_item(item_id, calories, item_name) values
(888388, 444388, 'Potassium Myristate');
insert into food_item(item_id, calories, item_name) values
(888389, 444389, 'Potassium Palmitate');
insert into food_item(item_id, calories, item_name) values
(888390, 444390, 'Potassium Stearate _ Stearic Acid Potas  sium Salt');
insert into food_item(item_id, calories, item_name) values
(888391, 444391, 'PPG-3-Myreth-11');
insert into food_item(item_id, calories, item_name) values
(888392, 444392, 'PPG-4-Ceteareth-12');
insert into food_item(item_id, calories, item_name) values
(888393, 444393, 'PPG-4-Ceteth-1, -5 or -10');
insert into food_item(item_id, calories, item_name) values
(888394, 444394, 'PPG-4 Myristyl Ether');
insert into food_item(item_id, calories, item_name) values
(888395, 444395, 'PPG-5-Ceteth- 10 Phosphate');
insert into food_item(item_id, calories, item_name) values
(888396, 444396, 'PPG-6-C12-18 Pareth');
insert into food_item(item_id, calories, item_name) values
(888397, 444397, 'PPG-8-Ceteth, -5, -10, or -20');
insert into food_item(item_id, calories, item_name) values
(888398, 444398, 'PPG-9-Steareth-3');
insert into food_item(item_id, calories, item_name) values
(888399, 444399, 'PPG-10-Ceteareth-20');
insert into food_item(item_id, calories, item_name) values
(888400, 444400, 'PPG-10 Cetyl Ether leyl Ether');
insert into food_item(item_id, calories, item_name) values
(888401, 444401, 'PPG-11 or -15 Stearyl Ether');
insert into food_item(item_id, calories, item_name) values
(888402, 444402, 'PPG-26 Oleate _ Polyxypropylene 2000 Monooleate; Carbowax');
insert into food_item(item_id, calories, item_name) values
(888403, 444403, 'PPG-28 Cetyl Ether');
insert into food_item(item_id, calories, item_name) values
(888404, 444404, 'PPG-30 Cetyl Ether');
insert into food_item(item_id, calories, item_name) values
(888405, 444405, 'PPG-30, -50, Oleyl Ether');
insert into food_item(item_id, calories, item_name) values
(888406, 444406, 'PPG-36 Oleate _ Polyoxypropylene (36)Monooleate');
insert into food_item(item_id, calories, item_name) values
(888407, 444407, 'PPG-Isocetyl Ether PPG-3- Isosteareth-9');
insert into food_item(item_id, calories, item_name) values
(888408, 444408, 'Proline');
insert into food_item(item_id, calories, item_name) values
(888409, 444409, 'Propylene Glycol Myristate');
insert into food_item(item_id, calories, item_name) values
(888410, 444410, 'Protein Fatty Acid Condensates');
insert into food_item(item_id, calories, item_name) values
(888411, 444411, 'Proteins');
insert into food_item(item_id, calories, item_name) values
(888412, 444412, 'Pyridium Compounds');
insert into food_item(item_id, calories, item_name) values
(888413, 444413, 'Pyroligneous Acid');
insert into food_item(item_id, calories, item_name) values
(888414, 444414, 'Retinyl Palmitate');
insert into food_item(item_id, calories, item_name) values
(888415, 444415, 'Ribonucleic Acid _ RNA');
insert into food_item(item_id, calories, item_name) values
(888416, 444416, 'Sarcosines');
insert into food_item(item_id, calories, item_name) values
(888417, 444417, 'S-Carboxy Methyl Cysteine');
insert into food_item(item_id, calories, item_name) values
(888418, 444418, 'Sebactic Acid  _ Decanedioic Acid');
insert into food_item(item_id, calories, item_name) values
(888419, 444419, 'Serine');
insert into food_item(item_id, calories, item_name) values
(888420, 444420, 'Skatole');
insert into food_item(item_id, calories, item_name) values
(888421, 444421, 'Sodium Aluminum Chloroydroxyl Lactate');
insert into food_item(item_id, calories, item_name) values
(888422, 444422, 'Sodium C12-15 Pareth-7  Carboxylate');
insert into food_item(item_id, calories, item_name) values
(888423, 444423, 'Sodium C12-15 Pareth-Sulfate');
insert into food_item(item_id, calories, item_name) values
(888424, 444424, 'Sodium Cetearyl Sulfate');
insert into food_item(item_id, calories, item_name) values
(888425, 444425, 'Sodium Cetyl Sulfate');
insert into food_item(item_id, calories, item_name) values
(888426, 444426, 'Sodium Cocyl Sarcosinate');
insert into food_item(item_id, calories, item_name) values
(888427, 444427, 'Sodium DNA');
insert into food_item(item_id, calories, item_name) values
(888428, 444428, 'Sodium Glyceryl Oleate Phosphate');
insert into food_item(item_id, calories, item_name) values
(888429, 444429, 'Sodium Isosteareth-6 Carboxylate');
insert into food_item(item_id, calories, item_name) values
(888430, 444430, 'Sodium Isosteroyl LacrylatE');
insert into food_item(item_id, calories, item_name) values
(888431, 444431, 'Sodium Myreth Sulfate');
insert into food_item(item_id, calories, item_name) values
(888432, 444432, 'Sodium Myristate');
insert into food_item(item_id, calories, item_name) values
(888433, 444433, 'Sodium Myristoyl Isethionate');
insert into food_item(item_id, calories, item_name) values
(888434, 444434, 'Sodium Myristoyl Sarcosinate');
insert into food_item(item_id, calories, item_name) values
(888435, 444435, 'Sodium Myristyl Sulfate');
insert into food_item(item_id, calories, item_name) values
(888436, 444436, 'Sodium Oleth-7 or -8 Phosphate');
insert into food_item(item_id, calories, item_name) values
(888437, 444437, 'Sodium Palmitate');
insert into food_item(item_id, calories, item_name) values
(888438, 444438, 'Sodium Pareth- 15-7 or 25-7 Carboxylate');
insert into food_item(item_id, calories, item_name) values
(888439, 444439, 'Sodium Pareth-23 or -25 Sulfate');
insert into food_item(item_id, calories, item_name) values
(888440, 444440, 'Sodium PCA');
insert into food_item(item_id, calories, item_name) values
(888441, 444441, 'Sodium PCA Methysilanol');
insert into food_item(item_id, calories, item_name) values
(888442, 444442, 'Sodium Ribonucleic Acid _ SRNA');
insert into food_item(item_id, calories, item_name) values
(888443, 444443, 'Sodium Sarcosinate ');
insert into food_item(item_id, calories, item_name) values
(888444, 444444, 'Sodium Soap');
insert into food_item(item_id, calories, item_name) values
(888445, 444445, 'Sodium Stearate');
insert into food_item(item_id, calories, item_name) values
(888446, 444446, 'Sodium Steroyl Lactylate');
insert into food_item(item_id, calories, item_name) values
(888447, 444447, 'Sodium Urocanate');
insert into food_item(item_id, calories, item_name) values
(888448, 444448, 'Sorbeth-6 Hexastearate');
insert into food_item(item_id, calories, item_name) values
(888449, 444449, 'Sorbitan Diisoseate');
insert into food_item(item_id, calories, item_name) values
(888450, 444450, 'Sorbitan Dioleate');
insert into food_item(item_id, calories, item_name) values
(888451, 444451, 'Sorbitan Fatty Acid Esters');
insert into food_item(item_id, calories, item_name) values
(888452, 444452, 'Sorbitan Isostearate');
insert into food_item(item_id, calories, item_name) values
(888453, 444453, 'Sorbitan Oleate _ Sorbitan Monooleate');
insert into food_item(item_id, calories, item_name) values
(888454, 444454, 'Sorbitan Palmitate _ Span 40 (TM)');
insert into food_item(item_id, calories, item_name) values
(888455, 444455, 'Sorbitan Sesquioleate');
insert into food_item(item_id, calories, item_name) values
(888456, 444456, 'Sorbitan Sequistearate');
insert into food_item(item_id, calories, item_name) values
(888457, 444457, 'Sorbitan Triisostearate');
insert into food_item(item_id, calories, item_name) values
(888458, 444458, 'Sorbitan Tristearate');
insert into food_item(item_id, calories, item_name) values
(888459, 444459, 'Spermaceti _ Cetyl Palmitate');
insert into food_item(item_id, calories, item_name) values
(888460, 444460, 'Stearalkonium Bentonite');
insert into food_item(item_id, calories, item_name) values
(888461, 444461, 'Stearalkonium Chloride');
insert into food_item(item_id, calories, item_name) values
(888462, 444462, 'Stearalkonium Hectorite');
insert into food_item(item_id, calories, item_name) values
(888463, 444463, 'Stearamide');
insert into food_item(item_id, calories, item_name) values
(888464, 444464, 'Stearamide DEA _ Stearic AcidDiethanolamide');
insert into food_item(item_id, calories, item_name) values
(888465, 444465, 'Stearamide DIBA Stearate');
insert into food_item(item_id, calories, item_name) values
(888466, 444466, 'Stearamide MIPA Stearate');
insert into food_item(item_id, calories, item_name) values
(888467, 444467, 'Stearamide MIPA');
insert into food_item(item_id, calories, item_name) values
(888468, 444468, 'Stearamide Oxide');
insert into food_item(item_id, calories, item_name) values
(888469, 444469, 'Stearmidopropalkonium Chloride');
insert into food_item(item_id, calories, item_name) values
(888470, 444470, 'Stearamidopropyl Dimethylamine');
insert into food_item(item_id, calories, item_name) values
(888471, 444471, 'Stearamine');
insert into food_item(item_id, calories, item_name) values
(888472, 444472, 'Stearamine Oxide');
insert into food_item(item_id, calories, item_name) values
(888473, 444473, 'Stearates');
insert into food_item(item_id, calories, item_name) values
(888474, 444474, 'Steareth-2');
insert into food_item(item_id, calories, item_name) values
(888475, 444475, 'Steareth-4 through -100');
insert into food_item(item_id, calories, item_name) values
(888476, 444476, 'Stearic Acid');
insert into food_item(item_id, calories, item_name) values
(888477, 444477, 'Stearic Hydrazide');
insert into food_item(item_id, calories, item_name) values
(888478, 444478, 'Stearmidoethyl Diethylamine');
insert into food_item(item_id, calories, item_name) values
(888479, 444479, 'Stearoamphoacetate');
insert into food_item(item_id, calories, item_name) values
(888480, 444480, 'Stearoamphocarboxyglycinate');
insert into food_item(item_id, calories, item_name) values
(888481, 444481, 'Stearoamphodiacetate');
insert into food_item(item_id, calories, item_name) values
(888482, 444482, 'Stearoamphohydroxypropysulfonate');
insert into food_item(item_id, calories, item_name) values
(888483, 444483, 'Stearoamphopropionate');
insert into food_item(item_id, calories, item_name) values
(888484, 444484, 'Stearone');
insert into food_item(item_id, calories, item_name) values
(888485, 444485, 'Stearoxy Dimethicone');
insert into food_item(item_id, calories, item_name) values
(888486, 444486, 'Stearoxytrimethylsilane');
insert into food_item(item_id, calories, item_name) values
(888487, 444487, 'Stearoyl Lactylic Acid');
insert into food_item(item_id, calories, item_name) values
(888488, 444488, 'Stearoyl Sarcosine');
insert into food_item(item_id, calories, item_name) values
(888489, 444489, 'Steartrimonium Chloride');
insert into food_item(item_id, calories, item_name) values
(888490, 444490, 'Steartrimonium Hydrolyzed Animal Protein');
insert into food_item(item_id, calories, item_name) values
(888491, 444491, 'Stearyl Acetate');
insert into food_item(item_id, calories, item_name) values
(888492, 444492, 'Stearyl Betaine');
insert into food_item(item_id, calories, item_name) values
(888493, 444493, 'Stearyl Caprylate');
insert into food_item(item_id, calories, item_name) values
(888494, 444494, 'Stearyl Citrate');
insert into food_item(item_id, calories, item_name) values
(888495, 444495, 'Stearyl Erucamide');
insert into food_item(item_id, calories, item_name) values
(888496, 444496, 'Stearyl Erucate');
insert into food_item(item_id, calories, item_name) values
(888497, 444497, 'Stearyl Ghycyrrhetinate');
insert into food_item(item_id, calories, item_name) values
(888498, 444498, 'Stearyl Heptanoate');
/*--Items--*/
insert into item(item_id, quantity, price) values(888000, 777000, 555000.0);
insert into item(item_id, quantity, price) values(888001, 777001, 555001.0);
insert into item(item_id, quantity, price) values(888002, 777002, 555002.0);
insert into item(item_id, quantity, price) values(888003, 777003, 555003.0);
insert into item(item_id, quantity, price) values(888004, 777004, 555004.0);
insert into item(item_id, quantity, price) values(888005, 777005, 555005.0);
insert into item(item_id, quantity, price) values(888006, 777006, 555006.0);
insert into item(item_id, quantity, price) values(888007, 777007, 555007.0);
insert into item(item_id, quantity, price) values(888008, 777008, 555008.0);
insert into item(item_id, quantity, price) values(888009, 777009, 555009.0);
insert into item(item_id, quantity, price) values(888010, 777010, 555010.0);
insert into item(item_id, quantity, price) values(888011, 777011, 555011.0);
insert into item(item_id, quantity, price) values(888012, 777012, 555012.0);
insert into item(item_id, quantity, price) values(888013, 777013, 555013.0);
insert into item(item_id, quantity, price) values(888014, 777014, 555014.0);
insert into item(item_id, quantity, price) values(888015, 777015, 555015.0);
insert into item(item_id, quantity, price) values(888016, 777016, 555016.0);
insert into item(item_id, quantity, price) values(888017, 777017, 555017.0);
insert into item(item_id, quantity, price) values(888018, 777018, 555018.0);
insert into item(item_id, quantity, price) values(888019, 777019, 555019.0);
insert into item(item_id, quantity, price) values(888020, 777020, 555020.0);
insert into item(item_id, quantity, price) values(888021, 777021, 555021.0);
insert into item(item_id, quantity, price) values(888022, 777022, 555022.0);
insert into item(item_id, quantity, price) values(888023, 777023, 555023.0);
insert into item(item_id, quantity, price) values(888024, 777024, 555024.0);
insert into item(item_id, quantity, price) values(888025, 777025, 555025.0);
insert into item(item_id, quantity, price) values(888026, 777026, 555026.0);
insert into item(item_id, quantity, price) values(888027, 777027, 555027.0);
insert into item(item_id, quantity, price) values(888028, 777028, 555028.0);
insert into item(item_id, quantity, price) values(888029, 777029, 555029.0);
insert into item(item_id, quantity, price) values(888030, 777030, 555030.0);
insert into item(item_id, quantity, price) values(888031, 777031, 555031.0);
insert into item(item_id, quantity, price) values(888032, 777032, 555032.0);
insert into item(item_id, quantity, price) values(888033, 777033, 555033.0);
insert into item(item_id, quantity, price) values(888034, 777034, 555034.0);
insert into item(item_id, quantity, price) values(888035, 777035, 555035.0);
insert into item(item_id, quantity, price) values(888036, 777036, 555036.0);
insert into item(item_id, quantity, price) values(888037, 777037, 555037.0);
insert into item(item_id, quantity, price) values(888038, 777038, 555038.0);
insert into item(item_id, quantity, price) values(888039, 777039, 555039.0);
insert into item(item_id, quantity, price) values(888040, 777040, 555040.0);
insert into item(item_id, quantity, price) values(888041, 777041, 555041.0);
insert into item(item_id, quantity, price) values(888042, 777042, 555042.0);
insert into item(item_id, quantity, price) values(888043, 777043, 555043.0);
insert into item(item_id, quantity, price) values(888044, 777044, 555044.0);
insert into item(item_id, quantity, price) values(888045, 777045, 555045.0);
insert into item(item_id, quantity, price) values(888046, 777046, 555046.0);
insert into item(item_id, quantity, price) values(888047, 777047, 555047.0);
insert into item(item_id, quantity, price) values(888048, 777048, 555048.0);
insert into item(item_id, quantity, price) values(888049, 777049, 555049.0);
insert into item(item_id, quantity, price) values(888050, 777050, 555050.0);
insert into item(item_id, quantity, price) values(888051, 777051, 555051.0);
insert into item(item_id, quantity, price) values(888052, 777052, 555052.0);
insert into item(item_id, quantity, price) values(888053, 777053, 555053.0);
insert into item(item_id, quantity, price) values(888054, 777054, 555054.0);
insert into item(item_id, quantity, price) values(888055, 777055, 555055.0);
insert into item(item_id, quantity, price) values(888056, 777056, 555056.0);
insert into item(item_id, quantity, price) values(888057, 777057, 555057.0);
insert into item(item_id, quantity, price) values(888058, 777058, 555058.0);
insert into item(item_id, quantity, price) values(888059, 777059, 555059.0);
insert into item(item_id, quantity, price) values(888060, 777060, 555060.0);
insert into item(item_id, quantity, price) values(888061, 777061, 555061.0);
insert into item(item_id, quantity, price) values(888062, 777062, 555062.0);
insert into item(item_id, quantity, price) values(888063, 777063, 555063.0);
insert into item(item_id, quantity, price) values(888064, 777064, 555064.0);
insert into item(item_id, quantity, price) values(888065, 777065, 555065.0);
insert into item(item_id, quantity, price) values(888066, 777066, 555066.0);
insert into item(item_id, quantity, price) values(888067, 777067, 555067.0);
insert into item(item_id, quantity, price) values(888068, 777068, 555068.0);
insert into item(item_id, quantity, price) values(888069, 777069, 555069.0);
insert into item(item_id, quantity, price) values(888070, 777070, 555070.0);
insert into item(item_id, quantity, price) values(888071, 777071, 555071.0);
insert into item(item_id, quantity, price) values(888072, 777072, 555072.0);
insert into item(item_id, quantity, price) values(888073, 777073, 555073.0);
insert into item(item_id, quantity, price) values(888074, 777074, 555074.0);
insert into item(item_id, quantity, price) values(888075, 777075, 555075.0);
insert into item(item_id, quantity, price) values(888076, 777076, 555076.0);
insert into item(item_id, quantity, price) values(888077, 777077, 555077.0);
insert into item(item_id, quantity, price) values(888078, 777078, 555078.0);
insert into item(item_id, quantity, price) values(888079, 777079, 555079.0);
insert into item(item_id, quantity, price) values(888080, 777080, 555080.0);
insert into item(item_id, quantity, price) values(888081, 777081, 555081.0);
insert into item(item_id, quantity, price) values(888082, 777082, 555082.0);
insert into item(item_id, quantity, price) values(888083, 777083, 555083.0);
insert into item(item_id, quantity, price) values(888084, 777084, 555084.0);
insert into item(item_id, quantity, price) values(888085, 777085, 555085.0);
insert into item(item_id, quantity, price) values(888086, 777086, 555086.0);
insert into item(item_id, quantity, price) values(888087, 777087, 555087.0);
insert into item(item_id, quantity, price) values(888088, 777088, 555088.0);
insert into item(item_id, quantity, price) values(888089, 777089, 555089.0);
insert into item(item_id, quantity, price) values(888090, 777090, 555090.0);
insert into item(item_id, quantity, price) values(888091, 777091, 555091.0);
insert into item(item_id, quantity, price) values(888092, 777092, 555092.0);
insert into item(item_id, quantity, price) values(888093, 777093, 555093.0);
insert into item(item_id, quantity, price) values(888094, 777094, 555094.0);
insert into item(item_id, quantity, price) values(888095, 777095, 555095.0);
insert into item(item_id, quantity, price) values(888096, 777096, 555096.0);
insert into item(item_id, quantity, price) values(888097, 777097, 555097.0);
insert into item(item_id, quantity, price) values(888098, 777098, 555098.0);
insert into item(item_id, quantity, price) values(888099, 777099, 555099.0);
insert into item(item_id, quantity, price) values(888100, 777100, 555100.0);
insert into item(item_id, quantity, price) values(888101, 777101, 555101.0);
insert into item(item_id, quantity, price) values(888102, 777102, 555102.0);
insert into item(item_id, quantity, price) values(888103, 777103, 555103.0);
insert into item(item_id, quantity, price) values(888104, 777104, 555104.0);
insert into item(item_id, quantity, price) values(888105, 777105, 555105.0);
insert into item(item_id, quantity, price) values(888106, 777106, 555106.0);
insert into item(item_id, quantity, price) values(888107, 777107, 555107.0);
insert into item(item_id, quantity, price) values(888108, 777108, 555108.0);
insert into item(item_id, quantity, price) values(888109, 777109, 555109.0);
insert into item(item_id, quantity, price) values(888110, 777110, 555110.0);
insert into item(item_id, quantity, price) values(888111, 777111, 555111.0);
insert into item(item_id, quantity, price) values(888112, 777112, 555112.0);
insert into item(item_id, quantity, price) values(888113, 777113, 555113.0);
insert into item(item_id, quantity, price) values(888114, 777114, 555114.0);
insert into item(item_id, quantity, price) values(888115, 777115, 555115.0);
insert into item(item_id, quantity, price) values(888116, 777116, 555116.0);
insert into item(item_id, quantity, price) values(888117, 777117, 555117.0);
insert into item(item_id, quantity, price) values(888118, 777118, 555118.0);
insert into item(item_id, quantity, price) values(888119, 777119, 555119.0);
insert into item(item_id, quantity, price) values(888120, 777120, 555120.0);
insert into item(item_id, quantity, price) values(888121, 777121, 555121.0);
insert into item(item_id, quantity, price) values(888122, 777122, 555122.0);
insert into item(item_id, quantity, price) values(888123, 777123, 555123.0);
insert into item(item_id, quantity, price) values(888124, 777124, 555124.0);
insert into item(item_id, quantity, price) values(888125, 777125, 555125.0);
insert into item(item_id, quantity, price) values(888126, 777126, 555126.0);
insert into item(item_id, quantity, price) values(888127, 777127, 555127.0);
insert into item(item_id, quantity, price) values(888128, 777128, 555128.0);
insert into item(item_id, quantity, price) values(888129, 777129, 555129.0);
insert into item(item_id, quantity, price) values(888130, 777130, 555130.0);
insert into item(item_id, quantity, price) values(888131, 777131, 555131.0);
insert into item(item_id, quantity, price) values(888132, 777132, 555132.0);
insert into item(item_id, quantity, price) values(888133, 777133, 555133.0);
insert into item(item_id, quantity, price) values(888134, 777134, 555134.0);
insert into item(item_id, quantity, price) values(888135, 777135, 555135.0);
insert into item(item_id, quantity, price) values(888136, 777136, 555136.0);
insert into item(item_id, quantity, price) values(888137, 777137, 555137.0);
insert into item(item_id, quantity, price) values(888138, 777138, 555138.0);
insert into item(item_id, quantity, price) values(888139, 777139, 555139.0);
insert into item(item_id, quantity, price) values(888140, 777140, 555140.0);
insert into item(item_id, quantity, price) values(888141, 777141, 555141.0);
insert into item(item_id, quantity, price) values(888142, 777142, 555142.0);
insert into item(item_id, quantity, price) values(888143, 777143, 555143.0);
insert into item(item_id, quantity, price) values(888144, 777144, 555144.0);
insert into item(item_id, quantity, price) values(888145, 777145, 555145.0);
insert into item(item_id, quantity, price) values(888146, 777146, 555146.0);
insert into item(item_id, quantity, price) values(888147, 777147, 555147.0);
insert into item(item_id, quantity, price) values(888148, 777148, 555148.0);
insert into item(item_id, quantity, price) values(888149, 777149, 555149.0);
insert into item(item_id, quantity, price) values(888150, 777150, 555150.0);
insert into item(item_id, quantity, price) values(888151, 777151, 555151.0);
insert into item(item_id, quantity, price) values(888152, 777152, 555152.0);
insert into item(item_id, quantity, price) values(888153, 777153, 555153.0);
insert into item(item_id, quantity, price) values(888154, 777154, 555154.0);
insert into item(item_id, quantity, price) values(888155, 777155, 555155.0);
insert into item(item_id, quantity, price) values(888156, 777156, 555156.0);
insert into item(item_id, quantity, price) values(888157, 777157, 555157.0);
insert into item(item_id, quantity, price) values(888158, 777158, 555158.0);
insert into item(item_id, quantity, price) values(888159, 777159, 555159.0);
insert into item(item_id, quantity, price) values(888160, 777160, 555160.0);
insert into item(item_id, quantity, price) values(888161, 777161, 555161.0);
insert into item(item_id, quantity, price) values(888162, 777162, 555162.0);
insert into item(item_id, quantity, price) values(888163, 777163, 555163.0);
insert into item(item_id, quantity, price) values(888164, 777164, 555164.0);
insert into item(item_id, quantity, price) values(888165, 777165, 555165.0);
insert into item(item_id, quantity, price) values(888166, 777166, 555166.0);
insert into item(item_id, quantity, price) values(888167, 777167, 555167.0);
insert into item(item_id, quantity, price) values(888168, 777168, 555168.0);
insert into item(item_id, quantity, price) values(888169, 777169, 555169.0);
insert into item(item_id, quantity, price) values(888170, 777170, 555170.0);
insert into item(item_id, quantity, price) values(888171, 777171, 555171.0);
insert into item(item_id, quantity, price) values(888172, 777172, 555172.0);
insert into item(item_id, quantity, price) values(888173, 777173, 555173.0);
insert into item(item_id, quantity, price) values(888174, 777174, 555174.0);
insert into item(item_id, quantity, price) values(888175, 777175, 555175.0);
insert into item(item_id, quantity, price) values(888176, 777176, 555176.0);
insert into item(item_id, quantity, price) values(888177, 777177, 555177.0);
insert into item(item_id, quantity, price) values(888178, 777178, 555178.0);
insert into item(item_id, quantity, price) values(888179, 777179, 555179.0);
insert into item(item_id, quantity, price) values(888180, 777180, 555180.0);
insert into item(item_id, quantity, price) values(888181, 777181, 555181.0);
insert into item(item_id, quantity, price) values(888182, 777182, 555182.0);
insert into item(item_id, quantity, price) values(888183, 777183, 555183.0);
insert into item(item_id, quantity, price) values(888184, 777184, 555184.0);
insert into item(item_id, quantity, price) values(888185, 777185, 555185.0);
insert into item(item_id, quantity, price) values(888186, 777186, 555186.0);
insert into item(item_id, quantity, price) values(888187, 777187, 555187.0);
insert into item(item_id, quantity, price) values(888188, 777188, 555188.0);
insert into item(item_id, quantity, price) values(888189, 777189, 555189.0);
insert into item(item_id, quantity, price) values(888190, 777190, 555190.0);
insert into item(item_id, quantity, price) values(888191, 777191, 555191.0);
insert into item(item_id, quantity, price) values(888192, 777192, 555192.0);
insert into item(item_id, quantity, price) values(888193, 777193, 555193.0);
insert into item(item_id, quantity, price) values(888194, 777194, 555194.0);
insert into item(item_id, quantity, price) values(888195, 777195, 555195.0);
insert into item(item_id, quantity, price) values(888196, 777196, 555196.0);
insert into item(item_id, quantity, price) values(888197, 777197, 555197.0);
insert into item(item_id, quantity, price) values(888198, 777198, 555198.0);
insert into item(item_id, quantity, price) values(888199, 777199, 555199.0);
insert into item(item_id, quantity, price) values(888200, 777200, 555200.0);
insert into item(item_id, quantity, price) values(888201, 777201, 555201.0);
insert into item(item_id, quantity, price) values(888202, 777202, 555202.0);
insert into item(item_id, quantity, price) values(888203, 777203, 555203.0);
insert into item(item_id, quantity, price) values(888204, 777204, 555204.0);
insert into item(item_id, quantity, price) values(888205, 777205, 555205.0);
insert into item(item_id, quantity, price) values(888206, 777206, 555206.0);
insert into item(item_id, quantity, price) values(888207, 777207, 555207.0);
insert into item(item_id, quantity, price) values(888208, 777208, 555208.0);
insert into item(item_id, quantity, price) values(888209, 777209, 555209.0);
insert into item(item_id, quantity, price) values(888210, 777210, 555210.0);
insert into item(item_id, quantity, price) values(888211, 777211, 555211.0);
insert into item(item_id, quantity, price) values(888212, 777212, 555212.0);
insert into item(item_id, quantity, price) values(888213, 777213, 555213.0);
insert into item(item_id, quantity, price) values(888214, 777214, 555214.0);
insert into item(item_id, quantity, price) values(888215, 777215, 555215.0);
insert into item(item_id, quantity, price) values(888216, 777216, 555216.0);
insert into item(item_id, quantity, price) values(888217, 777217, 555217.0);
insert into item(item_id, quantity, price) values(888218, 777218, 555218.0);
insert into item(item_id, quantity, price) values(888219, 777219, 555219.0);
insert into item(item_id, quantity, price) values(888220, 777220, 555220.0);
insert into item(item_id, quantity, price) values(888221, 777221, 555221.0);
insert into item(item_id, quantity, price) values(888222, 777222, 555222.0);
insert into item(item_id, quantity, price) values(888223, 777223, 555223.0);
insert into item(item_id, quantity, price) values(888224, 777224, 555224.0);
insert into item(item_id, quantity, price) values(888225, 777225, 555225.0);
insert into item(item_id, quantity, price) values(888226, 777226, 555226.0);
insert into item(item_id, quantity, price) values(888227, 777227, 555227.0);
insert into item(item_id, quantity, price) values(888228, 777228, 555228.0);
insert into item(item_id, quantity, price) values(888229, 777229, 555229.0);
insert into item(item_id, quantity, price) values(888230, 777230, 555230.0);
insert into item(item_id, quantity, price) values(888231, 777231, 555231.0);
insert into item(item_id, quantity, price) values(888232, 777232, 555232.0);
insert into item(item_id, quantity, price) values(888233, 777233, 555233.0);
insert into item(item_id, quantity, price) values(888234, 777234, 555234.0);
insert into item(item_id, quantity, price) values(888235, 777235, 555235.0);
insert into item(item_id, quantity, price) values(888236, 777236, 555236.0);
insert into item(item_id, quantity, price) values(888237, 777237, 555237.0);
insert into item(item_id, quantity, price) values(888238, 777238, 555238.0);
insert into item(item_id, quantity, price) values(888239, 777239, 555239.0);
insert into item(item_id, quantity, price) values(888240, 777240, 555240.0);
insert into item(item_id, quantity, price) values(888241, 777241, 555241.0);
insert into item(item_id, quantity, price) values(888242, 777242, 555242.0);
insert into item(item_id, quantity, price) values(888243, 777243, 555243.0);
insert into item(item_id, quantity, price) values(888244, 777244, 555244.0);
insert into item(item_id, quantity, price) values(888245, 777245, 555245.0);
insert into item(item_id, quantity, price) values(888246, 777246, 555246.0);
insert into item(item_id, quantity, price) values(888247, 777247, 555247.0);
insert into item(item_id, quantity, price) values(888248, 777248, 555248.0);
/*--Ingredient--*/
insert into ingredients(recipe_id, item_id) values
(0, 888040);
insert into ingredients(recipe_id, item_id) values
(1, 888198);
insert into ingredients(recipe_id, item_id) values
(2, 888164);
insert into ingredients(recipe_id, item_id) values
(3, 888192);
insert into ingredients(recipe_id, item_id) values
(4, 888197);
insert into ingredients(recipe_id, item_id) values
(5, 888178);
insert into ingredients(recipe_id, item_id) values
(6, 888224);
insert into ingredients(recipe_id, item_id) values
(7, 888120);
insert into ingredients(recipe_id, item_id) values
(8, 888187);
insert into ingredients(recipe_id, item_id) values
(9, 888216);
insert into ingredients(recipe_id, item_id) values
(10, 888151);
insert into ingredients(recipe_id, item_id) values
(11, 888176);
insert into ingredients(recipe_id, item_id) values
(12, 888038);
insert into ingredients(recipe_id, item_id) values
(13, 888192);
insert into ingredients(recipe_id, item_id) values
(14, 888164);
insert into ingredients(recipe_id, item_id) values
(15, 888181);
insert into ingredients(recipe_id, item_id) values
(16, 888030);
insert into ingredients(recipe_id, item_id) values
(17, 888197);
insert into ingredients(recipe_id, item_id) values
(18, 888076);
insert into ingredients(recipe_id, item_id) values
(19, 888004);
insert into ingredients(recipe_id, item_id) values
(20, 888060);
insert into ingredients(recipe_id, item_id) values
(21, 888056);
insert into ingredients(recipe_id, item_id) values
(22, 888148);
insert into ingredients(recipe_id, item_id) values
(23, 888075);
insert into ingredients(recipe_id, item_id) values
(24, 888187);
insert into ingredients(recipe_id, item_id) values
(25, 888035);
insert into ingredients(recipe_id, item_id) values
(26, 888215);
insert into ingredients(recipe_id, item_id) values
(27, 888001);
insert into ingredients(recipe_id, item_id) values
(28, 888108);
insert into ingredients(recipe_id, item_id) values
(29, 888104);
insert into ingredients(recipe_id, item_id) values
(30, 888062);
insert into ingredients(recipe_id, item_id) values
(31, 888058);
insert into ingredients(recipe_id, item_id) values
(32, 888192);
insert into ingredients(recipe_id, item_id) values
(33, 888178);
insert into ingredients(recipe_id, item_id) values
(34, 888122);
insert into ingredients(recipe_id, item_id) values
(35, 888053);
insert into ingredients(recipe_id, item_id) values
(36, 888249);
insert into ingredients(recipe_id, item_id) values
(37, 888187);
insert into ingredients(recipe_id, item_id) values
(38, 888174);
insert into ingredients(recipe_id, item_id) values
(39, 888240);
insert into ingredients(recipe_id, item_id) values
(40, 888124);
insert into ingredients(recipe_id, item_id) values
(41, 888202);
insert into ingredients(recipe_id, item_id) values
(42, 888199);
insert into ingredients(recipe_id, item_id) values
(43, 888162);
insert into ingredients(recipe_id, item_id) values
(44, 888144);
insert into ingredients(recipe_id, item_id) values
(45, 888219);
insert into ingredients(recipe_id, item_id) values
(46, 888202);
insert into ingredients(recipe_id, item_id) values
(47, 888034);
insert into ingredients(recipe_id, item_id) values
(48, 888195);
insert into ingredients(recipe_id, item_id) values
(49, 888092);
insert into ingredients(recipe_id, item_id) values
(50, 888067);
insert into ingredients(recipe_id, item_id) values
(51, 888036);
insert into ingredients(recipe_id, item_id) values
(52, 888002);
insert into ingredients(recipe_id, item_id) values
(53, 888207);
insert into ingredients(recipe_id, item_id) values
(54, 888145);
insert into ingredients(recipe_id, item_id) values
(55, 888093);
insert into ingredients(recipe_id, item_id) values
(56, 888105);
insert into ingredients(recipe_id, item_id) values
(57, 888067);
insert into ingredients(recipe_id, item_id) values
(58, 888179);
insert into ingredients(recipe_id, item_id) values
(59, 888105);
insert into ingredients(recipe_id, item_id) values
(60, 888208);
insert into ingredients(recipe_id, item_id) values
(61, 888102);
insert into ingredients(recipe_id, item_id) values
(62, 888123);
insert into ingredients(recipe_id, item_id) values
(63, 888146);
insert into ingredients(recipe_id, item_id) values
(64, 888055);
insert into ingredients(recipe_id, item_id) values
(65, 888139);
insert into ingredients(recipe_id, item_id) values
(66, 888223);
insert into ingredients(recipe_id, item_id) values
(67, 888000);
insert into ingredients(recipe_id, item_id) values
(68, 888132);
insert into ingredients(recipe_id, item_id) values
(69, 888066);
insert into ingredients(recipe_id, item_id) values
(70, 888007);
insert into ingredients(recipe_id, item_id) values
(71, 888041);
insert into ingredients(recipe_id, item_id) values
(72, 888139);
insert into ingredients(recipe_id, item_id) values
(73, 888125);
insert into ingredients(recipe_id, item_id) values
(74, 888212);
insert into ingredients(recipe_id, item_id) values
(75, 888192);
insert into ingredients(recipe_id, item_id) values
(76, 888133);
insert into ingredients(recipe_id, item_id) values
(77, 888225);
insert into ingredients(recipe_id, item_id) values
(78, 888074);
insert into ingredients(recipe_id, item_id) values
(79, 888035);
insert into ingredients(recipe_id, item_id) values
(80, 888082);
insert into ingredients(recipe_id, item_id) values
(81, 888247);
insert into ingredients(recipe_id, item_id) values
(82, 888234);
insert into ingredients(recipe_id, item_id) values
(83, 888036);
insert into ingredients(recipe_id, item_id) values
(84, 888147);
insert into ingredients(recipe_id, item_id) values
(85, 888020);
insert into ingredients(recipe_id, item_id) values
(86, 888103);
insert into ingredients(recipe_id, item_id) values
(87, 888205);
insert into ingredients(recipe_id, item_id) values
(88, 888165);
insert into ingredients(recipe_id, item_id) values
(89, 888204);
insert into ingredients(recipe_id, item_id) values
(90, 888028);
insert into ingredients(recipe_id, item_id) values
(91, 888045);
insert into ingredients(recipe_id, item_id) values
(92, 888170);
insert into ingredients(recipe_id, item_id) values
(93, 888227);
insert into ingredients(recipe_id, item_id) values
(94, 888166);
insert into ingredients(recipe_id, item_id) values
(95, 888225);
insert into ingredients(recipe_id, item_id) values
(96, 888112);
insert into ingredients(recipe_id, item_id) values
(97, 888057);
insert into ingredients(recipe_id, item_id) values
(98, 888009);
insert into ingredients(recipe_id, item_id) values
(99, 888234);
insert into ingredients(recipe_id, item_id) values
(100, 888176);
insert into ingredients(recipe_id, item_id) values
(101, 888043);
insert into ingredients(recipe_id, item_id) values
(102, 888217);
insert into ingredients(recipe_id, item_id) values
(103, 888195);
insert into ingredients(recipe_id, item_id) values
(104, 888025);
insert into ingredients(recipe_id, item_id) values
(105, 888211);
insert into ingredients(recipe_id, item_id) values
(106, 888209);
insert into ingredients(recipe_id, item_id) values
(107, 888104);
insert into ingredients(recipe_id, item_id) values
(108, 888170);
insert into ingredients(recipe_id, item_id) values
(109, 888168);
insert into ingredients(recipe_id, item_id) values
(110, 888121);
insert into ingredients(recipe_id, item_id) values
(111, 888109);
insert into ingredients(recipe_id, item_id) values
(112, 888161);
insert into ingredients(recipe_id, item_id) values
(113, 888084);
insert into ingredients(recipe_id, item_id) values
(114, 888181);
insert into ingredients(recipe_id, item_id) values
(115, 888215);
insert into ingredients(recipe_id, item_id) values
(116, 888134);
insert into ingredients(recipe_id, item_id) values
(117, 888115);
insert into ingredients(recipe_id, item_id) values
(118, 888094);
insert into ingredients(recipe_id, item_id) values
(119, 888124);
insert into ingredients(recipe_id, item_id) values
(120, 888023);
insert into ingredients(recipe_id, item_id) values
(121, 888161);
insert into ingredients(recipe_id, item_id) values
(122, 888226);
insert into ingredients(recipe_id, item_id) values
(123, 888019);
insert into ingredients(recipe_id, item_id) values
(124, 888014);
insert into ingredients(recipe_id, item_id) values
(125, 888118);
insert into ingredients(recipe_id, item_id) values
(126, 888148);
insert into ingredients(recipe_id, item_id) values
(127, 888244);
insert into ingredients(recipe_id, item_id) values
(128, 888173);
insert into ingredients(recipe_id, item_id) values
(129, 888065);
insert into ingredients(recipe_id, item_id) values
(130, 888087);
insert into ingredients(recipe_id, item_id) values
(131, 888010);
insert into ingredients(recipe_id, item_id) values
(132, 888037);
insert into ingredients(recipe_id, item_id) values
(133, 888133);
insert into ingredients(recipe_id, item_id) values
(134, 888019);
insert into ingredients(recipe_id, item_id) values
(135, 888052);
insert into ingredients(recipe_id, item_id) values
(136, 888215);
insert into ingredients(recipe_id, item_id) values
(137, 888197);
insert into ingredients(recipe_id, item_id) values
(138, 888026);
insert into ingredients(recipe_id, item_id) values
(139, 888114);
insert into ingredients(recipe_id, item_id) values
(140, 888069);
insert into ingredients(recipe_id, item_id) values
(141, 888101);
insert into ingredients(recipe_id, item_id) values
(142, 888008);
insert into ingredients(recipe_id, item_id) values
(143, 888217);
insert into ingredients(recipe_id, item_id) values
(144, 888176);
insert into ingredients(recipe_id, item_id) values
(145, 888076);
insert into ingredients(recipe_id, item_id) values
(146, 888115);
insert into ingredients(recipe_id, item_id) values
(147, 888181);
insert into ingredients(recipe_id, item_id) values
(148, 888001);
insert into ingredients(recipe_id, item_id) values
(149, 888136);
insert into ingredients(recipe_id, item_id) values
(150, 888198);
insert into ingredients(recipe_id, item_id) values
(151, 888199);
insert into ingredients(recipe_id, item_id) values
(152, 888073);
insert into ingredients(recipe_id, item_id) values
(153, 888246);
insert into ingredients(recipe_id, item_id) values
(154, 888181);
insert into ingredients(recipe_id, item_id) values
(155, 888213);
insert into ingredients(recipe_id, item_id) values
(156, 888131);
insert into ingredients(recipe_id, item_id) values
(157, 888123);
insert into ingredients(recipe_id, item_id) values
(158, 888089);
insert into ingredients(recipe_id, item_id) values
(159, 888100);
insert into ingredients(recipe_id, item_id) values
(160, 888201);
insert into ingredients(recipe_id, item_id) values
(161, 888242);
insert into ingredients(recipe_id, item_id) values
(162, 888087);
insert into ingredients(recipe_id, item_id) values
(163, 888019);
insert into ingredients(recipe_id, item_id) values
(164, 888158);
insert into ingredients(recipe_id, item_id) values
(165, 888151);
insert into ingredients(recipe_id, item_id) values
(166, 888202);
insert into ingredients(recipe_id, item_id) values
(167, 888146);
insert into ingredients(recipe_id, item_id) values
(168, 888118);
insert into ingredients(recipe_id, item_id) values
(169, 888234);
insert into ingredients(recipe_id, item_id) values
(170, 888065);
insert into ingredients(recipe_id, item_id) values
(171, 888229);
insert into ingredients(recipe_id, item_id) values
(172, 888054);
insert into ingredients(recipe_id, item_id) values
(173, 888194);
insert into ingredients(recipe_id, item_id) values
(174, 888077);
insert into ingredients(recipe_id, item_id) values
(175, 888147);
insert into ingredients(recipe_id, item_id) values
(176, 888141);
insert into ingredients(recipe_id, item_id) values
(177, 888242);
insert into ingredients(recipe_id, item_id) values
(178, 888084);
insert into ingredients(recipe_id, item_id) values
(179, 888116);
insert into ingredients(recipe_id, item_id) values
(180, 888122);
insert into ingredients(recipe_id, item_id) values
(181, 888038);
insert into ingredients(recipe_id, item_id) values
(182, 888007);
insert into ingredients(recipe_id, item_id) values
(183, 888048);
insert into ingredients(recipe_id, item_id) values
(184, 888098);
insert into ingredients(recipe_id, item_id) values
(185, 888178);
insert into ingredients(recipe_id, item_id) values
(186, 888170);
insert into ingredients(recipe_id, item_id) values
(187, 888141);
insert into ingredients(recipe_id, item_id) values
(188, 888185);
insert into ingredients(recipe_id, item_id) values
(189, 888094);
insert into ingredients(recipe_id, item_id) values
(190, 888063);
insert into ingredients(recipe_id, item_id) values
(191, 888018);
insert into ingredients(recipe_id, item_id) values
(192, 888249);
insert into ingredients(recipe_id, item_id) values
(193, 888176);
insert into ingredients(recipe_id, item_id) values
(194, 888009);
insert into ingredients(recipe_id, item_id) values
(195, 888067);
insert into ingredients(recipe_id, item_id) values
(196, 888232);
insert into ingredients(recipe_id, item_id) values
(197, 888034);
insert into ingredients(recipe_id, item_id) values
(198, 888149);
insert into created_user(user_id, email) values
(3300000, 'ida.Channa@university.com');
insert into created_user(user_id, email) values
(3300001, 'pattin.Bass@icloud.com');
insert into created_user(user_id, email) values
(3300002, 'marven.Gelling@icloud.com');
