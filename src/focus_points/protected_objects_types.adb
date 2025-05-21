with Ada.Text_IO; use Ada.Text_IO;

procedure Protected_Objects_Types is

   -- Define a protected type for a safe, shared counter
   protected type Safe_Counter is
      procedure Increment;
      function Get return Integer;
   private
      Count : Integer := 0;
   end Safe_Counter;

   -- Implementation of the protected type
   protected body Safe_Counter is

      procedure Increment is
      begin
         Count := Count + 1;
      end Increment;

      function Get return Integer is
      begin
         return Count;
      end Get;

   end Safe_Counter;

   -- Create a shared instance of the protected counter
   Counter : Safe_Counter;

   -- Task 1: increments the counter 5 times
   task Worker_1;
   task body Worker_1 is
   begin
      for I in 1 .. 5 loop
         Counter.Increment;
         delay 0.3;
      end loop;
   end Worker_1;

   -- Task 2: increments the counter 5 times
   task Worker_2;
   task body Worker_2 is
   begin
      for I in 1 .. 5 loop
         Counter.Increment;
         delay 0.2;
      end loop;
   end Worker_2;

begin
   -- Main program waits for both tasks to finish
   delay 1.5;
   Put_Line("Final counter value: " & Integer'Image(Counter.Get));

end Protected_Objects_Types;