with Ada.Text_IO; use Ada.Text_IO;

procedure Show_Predicates is
   pragma Assertion_Policy (Check);

   type Week is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

   subtype Work_Week is Week range Mon .. Fri;

   subtype Test_Days is Work_Week
   with Static_Predicate => Test_Days in Mon | Wed | Fri;

   type Tests_Week is array (Week) of Natural
   with
     Dynamic_Predicate =>
       (for all I in Tests_Week'Range
        => (case I is
              when Test_Days => Tests_Week (I) > 0,
              when others => Tests_Week (I) = 0));

   Num_Tests : Tests_Week :=
     (Mon => 3, Tue => 0, Wed => 4, Thu => 0, Fri => 2, Sat => 0, Sun => 0);

   procedure Display_Tests (N : Tests_Week) is
   begin
      for I in Test_Days loop
         Put_Line
           ("# tests on "
            & Test_Days'Image (I)
            & " => "
            & Integer'Image (N (I)));
      end loop;
   end Display_Tests;

begin
   Display_Tests (Num_Tests);

   --  Assigning non-conformant values to
   --  individual elements of the Tests_Week
   --  type does not trigger a predicate
   --  check:
   Num_Tests (Tue) := 2;

   --  However, assignments with the "complete"
   --  Tests_Week type trigger a predicate
   --  check. For example:
   --
   --  Num_Tests := (others => 0);

   --  Also, calling any subprogram with
   --  parameters of Tests_Week type
   --  triggers a predicate check. Therefore,
   --  the following line will fail:
   Display_Tests (Num_Tests);
end Show_Predicates;
