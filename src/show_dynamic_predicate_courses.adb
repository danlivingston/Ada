with Ada.Calendar; use Ada.Calendar;

with Ada.Containers.Vectors;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Show_Dynamic_Predicate_Courses is
   pragma Assertion_Policy (Check);
   package Courses is
      type Course_Container is private;

      type Course is record
         Name       : Unbounded_String;
         Start_Date : Time;
         End_Date   : Time;
      end record
      with Dynamic_Predicate => Course.Start_Date <= Course.End_Date;

      procedure Add (CC : in out Course_Container; C : Course);
   private
      package Course_Vectors is new
        Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Course);

      type Course_Container is record
         V : Course_Vectors.Vector;
      end record;
   end Courses;

   package body Courses is
      procedure Add (CC : in out Course_Container; C : Course) is
      begin
         CC.V.Append (C);
      end Add;
   end Courses;

   use Courses;

   CC : Course_Container;
begin
   Add
     (CC,
      Course'
        (Name       => To_Unbounded_String ("Intro to Photography"),
         Start_Date => Time_Of (2018, 5, 1),
         End_Date   => Time_Of (2018, 5, 10)));

   --  This should trigger an error in the
   --  dynamic predicate check
   Add
     (CC,
      Course'
        (Name       => To_Unbounded_String ("Intro to Video Recording"),
         Start_Date => Time_Of (2019, 5, 1),
         End_Date   => Time_Of (2018, 5, 10)));

end Show_Dynamic_Predicate_Courses;
