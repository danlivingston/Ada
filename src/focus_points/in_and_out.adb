with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure In_And_Out is

   -- This example defines a procedure that takes:
   --
   -- - an `in` parameter: a number that will be doubled,
   -- - an `in out` parameter: a counter that tracks how many times the procedure has been called,
   -- - an `out` parameter: a message string that describes the result of the operation.
   --
   -- When the procedure is called:
   --
   -- - The input number is read but not modified (ensured by `in` mode).
   -- - The counter is incremented and retained across calls (thanks to `in out` mode).
   -- - The message is assigned a new string and returned to the caller (`out` mode).
   --
   -- This example illustrates the clear separation of roles for each parameter — a principle that helps prevent
   -- unintended side effects and improves code readability and correctness.

   -- Procedure that doubles a number, counts the operation,
   -- and returns a message
   procedure Double_And_Track
     (Number : in Integer; Count : in out Integer; Msg : out Unbounded_String)
   is
   begin
      -- Example of invalid modification of an `in` parameter (will cause compile-time error)
      -- ERROR: 'Number' is mode 'in' and cannot be assigned
      -- Number := Number + 1;  

      -- Increment the counter (in out: read and write)
      Count := Count + 1;

      -- Set the message (out: write only)
      Msg :=
        To_Unbounded_String
          ("Doubled value is: " & Integer'Image (Number * 2));

   end Double_And_Track;

   -- Variables
   Input_Value : Integer := 5;                -- input
   Call_Count  : Integer := 0;                -- in out
   Result_Msg  : Unbounded_String;            -- out

begin

   -- First call
   Double_And_Track (Input_Value, Call_Count, Result_Msg);
   Put_Line (To_String (Result_Msg));
   Put_Line ("Calls so far: " & Integer'Image (Call_Count));

   -- Second call
   Double_And_Track (7, Call_Count, Result_Msg);
   Put_Line (To_String (Result_Msg));
   Put_Line ("Calls so far: " & Integer'Image (Call_Count));

-- Expected Result:
-- Doubled value is:  10
-- Calls so far:  1
-- Doubled value is:  14
-- Calls so far:  2

end In_And_Out;
