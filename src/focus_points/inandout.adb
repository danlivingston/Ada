with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure InAndOut is

   -- Procedure that doubles a number, counts the operation,
   -- and returns a message
   procedure Double_And_Track(Number : in     Integer;
                              Count  : in out Integer;
                              Msg    : out    Unbounded_String) is
   begin
      
      -- Increment the counter (in out: read and write)
      Count := Count + 1;

      -- Set the message (out: write only)
      Msg := To_Unbounded_String("Doubled value is: " & Integer'Image(Number * 2));
   
   end Double_And_Track;

   -- Variables
   Input_Value : Integer := 5;                -- input
   Call_Count  : Integer := 0;                -- in out
   Result_Msg  : Unbounded_String;            -- out

begin
   
   -- First call
   Double_And_Track(Input_Value, Call_Count, Result_Msg);
   Put_Line(To_String(Result_Msg));
   Put_Line("Calls so far: " & Integer'Image(Call_Count));

   -- Second call
   Double_And_Track(7, Call_Count, Result_Msg);
   Put_Line(To_String(Result_Msg));
   Put_Line("Calls so far: " & Integer'Image(Call_Count));

   -- Expected Result:
   -- Doubled value is:  10
   -- Calls so far:  1
   -- Doubled value is:  14
   -- Calls so far:  2

end InAndOut;