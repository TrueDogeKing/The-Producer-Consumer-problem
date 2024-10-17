-- A skeleton of an ADA program for an assignment in programming languages

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


procedure Simulation is

   ----GLOBAL VARIABLES---

   Number_Of_Producers: constant Integer := 5;
   Number_Of_Assemblies: constant Integer := 3;
   Number_Of_Consumers: constant Integer := 2;

   subtype Producer_Type is Integer range 1 .. Number_Of_Producers;
   subtype Assembly_Type is Integer range 1 .. Number_Of_Assemblies;
   subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;


   --each Producer is assigned a Product that it produces
   Product_Name: constant array (Producer_Type) of Unbounded_String
   := (To_Unbounded_String("Leg"),
       To_Unbounded_String("Countertop"),
       To_Unbounded_String("Seat"),
       To_Unbounded_String("Backrest"),
       To_Unbounded_String("Plank"));

Assembly_Name: constant array (Assembly_Type) of Unbounded_String
   := (To_Unbounded_String("Chair"),
       To_Unbounded_String("Table"),
       To_Unbounded_String("Drawer"));


   ----TASK DECLARATIONS----

   -- Producer produces determined product
   task type Producer is
      --delay 1.0;
      entry Start(Product: in Producer_Type; Production_Time: in Integer;quantity: in Integer);
   end Producer;

   -- Consumer gets an arbitrary assembly of several products from the buffer
   -- but he/she orders it randomly
   task type Consumer is
      entry Start(Consumer_Number: in Consumer_Type;
                  Consumption_Time: in Integer);
   end Consumer;


   task type Calendar is
      entry Start;
   end Calendar;

   -- Buffer receives products from Producers and delivers Assemblies to Consumers
   task type Buffer is
      -- Accept a product to the storage (provided there is a room for it)
      entry Take(Product: in Producer_Type; Number: in Integer);
      -- Deliver an assembly (provided there are enough products for it)
      entry Deliver(Assembly: in Assembly_Type; Number: out Integer);
      entry Sunday;
   end Buffer;

   P: array ( 1 .. Number_Of_Producers ) of Producer;
   K: array ( 1 .. Number_Of_Consumers ) of Consumer;
   B: Buffer;


   ----TASK DEFINITIONS----

   --Producer--
   -- Remake producer how about delay 1 ( so the deliveries would be every day ) the amount is still to be discussed

   task body Producer is
   -- Fixed production amounts for each type of product (per "day")
   subtype Production_Time_Range is Integer range 1 .. 3;
   package Random_Production is new Ada.Numerics.Discrete_Random(Production_Time_Range);
   G: Random_Production.Generator;

   Producer_Type_Number: Integer;
   Product_Number: Integer;  -- This tracks the total number of products produced.
   Production: Integer;

   -- Production amounts (quantities to be produced per day)
   Production_Amount: Integer;
begin
   -- Accept initial start call
   accept Start(Product: in Producer_Type; Production_Time: in Integer; quantity: in Integer) do
      Random_Production.Reset(G);
      Product_Number := quantity;  -- Initial quantity of the product produced
      Producer_Type_Number := Product;
      Production := Production_Time;
   end Start;

   -- Output to indicate the producer started
   Put_Line(ESC & "[93m" & "P: Started producer of " & To_String(Product_Name(Producer_Type_Number)) & ESC & "[0m");

   -- Loop to simulate daily production and delivery
   loop
      -- Determine the production amount based on the product type
      case Producer_Type_Number is
         when 1 => Production_Amount := 4;  -- Legs
         when 2 => Production_Amount := 1;  -- Countertop
         when 3 => Production_Amount := 1;  -- Seat
         when 4 => Production_Amount := 1;  -- Backrest
         when 5 => Production_Amount := 4;  -- Plank
         when others => Production_Amount := 1;  -- Default case (not strictly needed)
      end case;

      -- Produce and deliver the specified quantity
      for I in 1 .. Production_Amount loop
         Put_Line(ESC & "[93m" & "P: Produced product " & To_String(Product_Name(Producer_Type_Number))
                  & " number " & Integer'Image(Product_Number) & ESC & "[0m");

         -- Simulate storing the product in the buffer
         B.Take(Producer_Type_Number, Product_Number);
         Product_Number := Product_Number + 1;
      end loop;

      -- Simulate one "day" passing (1 second per day)
      delay 1.0;

   end loop;
end Producer;



   --Consumer--

   task body Consumer is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
      package Random_Consumption is new
        Ada.Numerics.Discrete_Random(Consumption_Time_Range);

      --each Consumer takes any (random) Assembly from the Buffer
      package Random_Assembly is new
        Ada.Numerics.Discrete_Random(Assembly_Type);

      G: Random_Consumption.Generator;
      GA: Random_Assembly.Generator;
      Consumer_Nb: Consumer_Type;
      Assembly_Number: Integer;
      Consumption: Integer;
      Assembly_Type: Integer;
      Consumer_Name: constant array (1 .. Number_Of_Consumers)
        of String(1 .. 9)
        := ("Consumer1", "Consumer2");
   begin
      accept Start(Consumer_Number: in Consumer_Type;
                   Consumption_Time: in Integer) do
         Random_Consumption.Reset(G);
         Random_Assembly.Reset(GA);
         Consumer_Nb := Consumer_Number;
         Consumption := Consumption_Time;
      end Start;
      Put_Line(ESC & "[96m" & "C: Started consumer " & Consumer_Name(Consumer_Nb) & ESC & "[0m");
      loop
      -- Simulate consumption time
         delay Duration(Random_Consumption.Random(G));
         Put_Line(ESC & "[96m" & "C: " & Consumer_Name(Consumer_Nb) & " Waiting consumer " & ESC & "[0m");

      -- Randomly choose an assembly for the consumer
      Assembly_Type := Random_Assembly.Random(GA);

      -- Take the assembly from the buffer
      B.Deliver(Assembly_Type, Assembly_Number);

      -- Debug output: what assembly and components the consumer takes
      Put_Line(ESC & "[96m" & "C: " & Consumer_Name(Consumer_Nb) & " takes assembly " &
         To_String(Assembly_Name(Assembly_Type)) & " number " & Integer'Image(Assembly_Number) & ESC & "[0m");



      end loop;
   end Consumer;

   ------------------------------------------------------------------------------------------

   task body Calendar is
      day_number : Integer := 1;  -- Initialize the day number
      Day_Duration : constant Duration := 1.0;  -- Duration of one day (1 second)
   begin
      accept Start do
         Put_Line("Calendar started.");
      end Start;

      loop
         delay Day_Duration;  -- Simulate the passing of one day
         Put_Line("Day: " & Integer'Image(day_number));

         if day_number = 7 then
            -- Call the Sunday operation in the Buffer task
            Put_Line("It's Sunday! Calling Buffer Sunday...");
            B.Sunday;  -- Assume Buffer has an entry Sunday to handle the special Sunday operation
            day_number := 1;  -- Reset the day number after Sunday
         else
            day_number := day_number + 1;  -- Increment the day number
         end if;
      end loop;
   end Calendar;



   --Buffer--

   task body Buffer is
      Storage_Capacity: constant Integer := 30;
      type Storage_type is array (Producer_Type) of Integer;
      Storage: Storage_type
        := (0, 0, 0, 0, 0);
      Assembly_Content: array (Assembly_Type, Producer_Type) of Integer
        := ((4, 0, 1, 1, 0),  -- Chair requires 4 legs, 1 seat, 1 backrest
            (4, 1, 0, 0, 0),  -- Table requires 4 legs, 1 countertop
            (0, 0, 0, 0, 4)); -- Drawer requires 4 planks
      Max_Assembly_Content: array(Producer_Type) of Integer;
      Assembly_Number: array(Assembly_Type) of Integer
        := (1, 1, 1);
      In_Storage: Integer := 0;

      procedure Today_Is_Sunday is
      begin
         Put_Line("B: Today is Sunday! Removing one of each product type from storage.");
         for W in Producer_Type loop
            if Storage(W) > 0 then
               Storage(W) := Storage(W) - 1;
               In_Storage := In_Storage - 1;
               Put_Line("B: Removed one " & To_String(Product_Name(W)) & ". Remaining: " & Integer'Image(Storage(W)));
            else
               Put_Line("B: No " & To_String(Product_Name(W)) & " in storage to remove.");


            end if;
         end loop;
      end Today_Is_Sunday;

      procedure Setup_Variables is
      begin
         for W in Producer_Type loop
            Max_Assembly_Content(W) := 0;
            for Z in Assembly_Type loop
               if Assembly_Content(Z, W) > Max_Assembly_Content(W) then
                  Max_Assembly_Content(W) := Assembly_Content(Z, W);
               end if;
            end loop;
         end loop;
      end Setup_Variables;

      function Can_Accept(Product: Producer_Type) return Boolean is
         -- add more advanced methods
      begin
         if In_Storage >= Storage_Capacity then
            return False;
         else
            return True;
         end if;
      end Can_Accept;

      function Can_Deliver(Assembly: Assembly_Type) return Boolean is
      begin
         for W in Producer_Type loop
            if Storage(W) < Assembly_Content(Assembly, W) then
               return False;
            end if;
         end loop;
         return True;
      end Can_Deliver;

      procedure Storage_Contents is
      begin

         Put_Line("|=== Storage contents ===|");
         for W in Producer_Type loop
            Put_Line("|  " & Integer'Image(Storage(W)) & " "
                     & To_String(Product_Name(W)));
         end loop;
         Put_Line("|   Number of products in storage: " & Integer'Image(In_Storage));
         Put_Line("|========================|");

      end Storage_Contents;

   begin
   Put_Line(ESC & "[91m" & "B: Buffer started" & ESC & "[0m");
   Setup_Variables;

      loop
         Storage_Contents;
         select
            accept Take(Product: in Producer_Type; Number: in Integer) do
               if Can_Accept(Product) then
                  Put_Line(ESC & "[91m" & "B: Accepted product " & To_String(Product_Name(Product)) & " number " &
                             Integer'Image(Number) & ESC & "[0m");
                  Storage(Product) := Storage(Product) + 1;
                  In_Storage := In_Storage + 1;
               else
                  Put_Line(ESC & "[91m" & "B: Rejected product " & To_String(Product_Name(Product)) & " number " &
                             Integer'Image(Number) & ESC & "[0m");
               end if;

            end Take;

         or

            accept Deliver(Assembly: in Assembly_Type; Number: out Integer) do
               if Can_Deliver(Assembly) then
                  Put_Line(ESC & "[91m" & "B: Delivered assembly " & To_String(Assembly_Name(Assembly)) & " number " &
                             Integer'Image(Assembly_Number(Assembly)) & ESC & "[0m");
                  for W in Producer_Type loop
                     Storage(W) := Storage(W) - Assembly_Content(Assembly, W);
                     In_Storage := In_Storage - Assembly_Content(Assembly, W);
                  end loop;
                  Number := Assembly_Number(Assembly);
                  Assembly_Number(Assembly) := Assembly_Number(Assembly) + 1;
               else
                  Put_Line(ESC & "[91m" & "B: Lacking products for assembly " & To_String(Assembly_Name(Assembly)) &
                             ESC & "[0m");
                  Number := 0;
               end if;
            end Deliver;

         or

            accept Sunday do
               Put_Line("B: It's Sunday. Performing Sunday operation.");
               Today_Is_Sunday;  -- Perform the Sunday operation to remove one of each product
            end Sunday;

         end select;
      end loop;
   end Buffer;



   ---"MAIN" FOR SIMULATION---


Cal: Calendar;  -- Declare the Calendar task here, globally

begin
   -- Start the Calendar task in the background
   Cal.Start;

   -- Start all Producer tasks
   for I in 1 .. Number_Of_Producers loop
      Put_Line("P: main function calling. I:" & Integer'Image(I) );
      P(I).Start(I, 10,2);
   end loop;

   -- Start all Consumer tasks
   for J in 1 .. Number_Of_Consumers loop
      K(J).Start(J, 12);
   end loop;

   -- The Calendar will continue running in the background,
   -- and producers and consumers will also run concurrently.

end Simulation;


