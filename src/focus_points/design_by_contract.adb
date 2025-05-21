with Ada.Text_IO; use Ada.Text_IO;

procedure Design_By_Contract is
   pragma
     Assertion_Policy
       (Check); -- Enable assertion checks (depending on the compiler, this may be different)
   -- Demonstrate the usage of a Bank package with contract checks
   package Bank is
      -- This package defines a simple bank account system
      type Money is new Float;
      type Bank_Account is tagged record
         Balance : Money := 0.0;
      end record;

      procedure Deposit (Account : in out Bank_Account; Amount : in Money)
      with
        Pre  => Amount > 0.0,
        Post => Account.Balance = Account.Balance'Old + Amount;

      procedure Withdraw (Account : in out Bank_Account; Amount : in Money)
      with
        Pre  => Amount > 0.0 and Amount <= Account.Balance,
        Post => Account.Balance = Account.Balance'Old - Amount;

      function Get_Balance (Account : Bank_Account) return Money;
   end Bank;

   package body Bank is
      -- Implementation of the Bank package
      procedure Deposit (Account : in out Bank_Account; Amount : in Money) is
      begin
         -- Comment out the actual implementation to simulate a contract check
         Account.Balance :=
           Account.Balance + Amount; -- This is the actual implementation
         null; -- Placeholder for contract check, when actual implementation is commented out
      end Deposit;

      procedure Withdraw (Account : in out Bank_Account; Amount : in Money) is
      begin
         Account.Balance := Account.Balance - Amount;
      end Withdraw;

      function Get_Balance (Account : Bank_Account) return Money is
      begin
         return Account.Balance;
      end Get_Balance;
   end Bank;

   use Bank;
   My_Account : Bank_Account;

begin
   Put_Line ("Initial balance: " & Money'Image (Get_Balance (My_Account)));

   Put_Line ("Depositing 100.0...");
   Deposit (My_Account, 100.0);
   Put_Line
     ("Balance after deposit: " & Money'Image (Get_Balance (My_Account)));

   Put_Line ("Withdrawing 30.0...");
   Withdraw (My_Account, 30.0);
   Put_Line
     ("Balance after withdrawal: " & Money'Image (Get_Balance (My_Account)));

   -- Uncommenting the lines below would raise a precondition violation
   --  Put_Line ("Attempting to withdraw 1000.0...");
   --  Withdraw (My_Account, 1000.0);

   Put_Line ("Done.");
end Design_By_Contract;
