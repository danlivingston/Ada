# Ada - PCP

> by Davide Ceresa & Dan Livingston

## Introduction

Ada is a high-level programming language designed for reliability, safety, and maintainability. Originally developed in the early 1980s by the U.S. Department of Defense, Ada was created to unify and standardize the many programming languages used in defense systems. It is named after Ada Lovelace, who is often regarded as the first computer programmer.

Ada is known for its strong typing, modularity, and built-in support for real-time and embedded systems. It is widely used in mission-critical applications such as aerospace, transportation, and medical devices—where software failures can have serious consequences.

One of Ada’s standout features is its emphasis on early error detection through strict compile-time checks, which helps prevent many common programming bugs. It also supports object-oriented and concurrent programming, making it suitable for complex and large-scale software systems.

In recent years, Ada has continued to evolve, with modern versions like Ada 2012 and Ada 2022 adding more expressive and flexible features while retaining its core focus on reliability and clarity.

### Installing Alire

This project uses [Alire](https://alire.ada.dev), the Ada package manager, to manage dependencies, compilation, and execution.

To get started, follow the official installation instructions here:  
👉 <https://alire.ada.dev/docs/#getting-started>

You can then verify the installation executing the following command:

```bash
alr --version
```

If you see the version number, you are good to go!

### Using this repo

After installing Alire, you can use this project by cloning this repo

```bash
    git clone https://github.com/danlivingston/Ada.git
```

After cloning this repository, run the following commands:

```bash
cd Ada        # Enter the project folder    §
alr build     # Builds the project and downloads dependencies
alr run       # Runs the executable
```

## Focus Topics

## 1. Declarative regions

In Ada, declarative regions are fundamental to the structure of the language. They are areas of code where you can declare entities such as variables, types, constants, procedures, functions, packages, etc. The concept is closely tied to block structure and scope. Understanding declarative regions helps you see where declarations are valid, what their lifetimes are, and how visibility is controlled.

### What Is a Declarative Region?

A declarative region in Ada is:

- The portion of code in which declarations are allowed and visible.
- Starts after a declare keyword (or the beginning of a block/package/subprogram) and ends before the begin keyword.
- Provides scope for identifiers (variables, types, etc.).
- Can be nested.

Declarative regions exist in:

- Procedures and functions
- Packages
- Tasks
- Blocks
- Loops (limited scope)
- Records and types

### Why Use Declarative Regions?

- Encapsulation: Keep variables local to a subprogram or block.
- Clarity: Organize code and limit scope of identifiers.
- Memory Management: Objects declared in a declarative region are automatically cleaned up at the end.
- Modularity: Each region can be self-contained with its own declarations.

### Example

A complete example demonstrating declarative regions in Ada can be found in:

```bash
src/focus_points/declarative_regions.adb
```

- Declarative_Regions is a procedure with its own declarative region (declaring X and Inner_Procedure).
- Inner_Procedure has its own declarative region (declaring Y).
- X is visible in Inner_Procedure due to nesting.

### Comparison with Java

```java
public class DeclarativeRegions {
    public static void main(String[] args) {
        int x = 10;

        class Inner {
            void display() {
                int y = x + 5; // x is visible here
                System.out.println("Y = " + y);
            }
        }

        Inner inner = new Inner();
        inner.display();
    }
}
```

- The Java inner class here emulates Ada's nested procedure.
- The variable x is effectively final or effectively final, similar to Ada's outer variable being visible in inner scope.

#### Key Differences

- Scope Control: Ada uses explicit declarative regions; Java uses implicit block scoping.
- Variable Lifetime: Ada variables are cleaned up when the region ends; Java relies on garbage collection.
- Nesting: Ada allows nested procedures/functions; Java does not (only inner classes or lambdas).
- Modularity: Ada emphasizes modularity through packages and nested blocks; Java uses classes and packages.
- Clarity: Ada’s explicit structure improves code readability and scope management.

## 2. In & Out Parameters

In Ada, subprograms (procedures and functions) use explicit parameter modes to define how data is passed and used within a call. These modes are: in, out, and in out. This design choice is central to Ada's philosophy of safety, clarity, and correctness.

### in Parameters

These are read-only parameters. The caller provides a value, and the callee may use it but cannot modify it.

```ada
procedure Display(Value : in Integer);
```

Attempting to write to Value in the procedure body would result in a compile-time error.

### out Parameters

These are write-only from the perspective of the callee. The caller does not expect any initial value, and the callee must assign a value before returning.

```ada
procedure Compute(Result : out Integer);
```

Reading from Result before assigning a value is a compile-time error.

### in out Parameters

These are read-write. The caller passes a value that the callee can both read and modify.

```ada
procedure Update(Counter : in out Integer);
```

This is suitable when the subprogram needs to modify and return a new version of a variable.

### Example

A full example demonstrating all three parameter modes (in, out, and in out) can be found in:

```bash
src/focus_points/in_and_outexample.adb
```

This example defines a procedure that takes:

- an in parameter: a number that will be doubled,
- an in out parameter: a counter that tracks how many times the procedure has been called,
- an out parameter: a message string that describes the result of the operation.

When the procedure is called:

- The input number is read but not modified (ensured by in mode).
- The counter is incremented and retained across calls (thanks to in out mode).
- The message is assigned a new string and returned to the caller (out mode).

This example illustrates the clear separation of roles for each parameter — a principle that helps prevent unintended side effects and improves code readability and correctness.

### Comparasion with Java

In Java, there is no way to explicitly declare if a parameter is used for input, output, or both.
All parameters are passed by value — but if you pass an object, Java copies the reference, so you can change the object’s content.

UNSURE: maybe at least the type of out is defined via void/int/etc.?

```java
class Data {
    public int value;
}

void update(Data d) {
    d.value = 42; // modifies the original object
}
```

This works because d refers to the same object.

But if you try the same with a primitive type like int:

```java
void update(int x) {
    x = 42;
}
```

Nothing changes outside the function — x is just a copy.

In summary:

- Java lets you modify objects, but not primitives.
- There’s no compiler check to ensure how parameters are used.
- In contrast, Ada forces you to be explicit (in, out, in out) — making data flow clear, safe, and verifiable.

### Useful Links

- <https://en.wikibooks.org/wiki/Ada_Programming/Subprograms>
- <https://learn.adacore.com/courses/Ada_For_The_CPP_Java_Developer/chapters/06_Functions_and_Procedures.html>

## 3. Design by Contract

Design by Contract (DbC) is a methodology that defines formal, precise, and verifiable interface specifications for software components. It involves:

- Preconditions – What must be true before a subprogram executes.
- Postconditions – What must be true after a subprogram executes.
- Invariants – Conditions that must always be true during the life of an object.

Ada natively supports Design by Contract starting from Ada 2012, using aspect specifications like Pre, Post, and Type_Invariant.

### Example

A complete example demonstrating Design by Contract in Ada can be found in:

```bash
src/focus_points/design_by_contract.adb
```

This example defines a simple class (type) representing a bank account. The class has:

- A precondition that ensures the deposit amount is positive.
- A postcondition that ensures the balance is updated correctly after a deposit.

When the deposit method is called, the precondition checks if the amount is valid. If not, an exception is raised. After the deposit, the postcondition checks if the balance has been updated correctly.

### Comparison with Java

Java doesn’t have built-in support for DbC, but similar behavior can be emulated using assertions, exceptions, or tools like Java Modeling Language (JML).

#### Java Equivalent (Manual)

```java
public class BankAccount {
    private double balance = 0.0;

    public void deposit(double amount) {
        assert amount > 0 : "Deposit must be positive";
        double oldBalance = balance;
        balance += amount;
        assert balance == oldBalance + amount : "Postcondition failed";
    }

    public void withdraw(double amount) {
        assert amount > 0 && amount <= balance : "Invalid withdraw amount";
        double oldBalance = balance;
        balance -= amount;
        assert balance == oldBalance - amount : "Postcondition failed";
    }

    public double getBalance() {
        return balance;
    }
}
```

In this Java example, assertions are used to check preconditions and postconditions. However, assertions can be disabled at runtime, making them less reliable than Ada's built-in contract checking.

## 4. Rendevous

In Ada, tasks are units of concurrent execution. The rendezvous mechanism allows tasks to communicate and synchronize by exchanging control and data in a safe and predictable way.

A rendezvous occurs when:

- A calling task invokes an entry of another task.
- The receiving task executes an accept statement for that entry.
- Both tasks wait for each other: the call and accept must happen at the same time.
- Once matched, the accept block executes and the tasks proceed.

This mechanism enforces safe synchronization between tasks, avoiding race conditions and ensuring clear communication patterns.

Core Features of Rendezvous

- Mutual Exclusion: only one task at a time can perform a rendezvous for a specific entry.
- Synchronization: both caller and callee block until they are both ready.
- Data Exchange: parameters can be passed to and from the entry during the rendezvous.
- Controlled Selection: with select, a task can choose between multiple possible rendezvous, handle timeouts, or terminate gracefully.

### Example

A complete example of a server task interacting with two client tasks using rendezvous is provided in:

```bash
src/focus_points/in_and_outexample.adb
```

This example defines:

- A Server task with an entry Print_Message.
- Two concurrent tasks (Client_1 and Client_2) that call Print_Message at different times.
- A select block in the Server that handles:
  - Incoming messages via accept
  - A timeout using delay

The rendezvous happens at each call to Print_Message, where the client task is blocked until the server accepts the message. Once both are ready, the server prints the message and both tasks continue.

### Visual Explanation

This sequence diagram shows how Client_1 and Client_2 interact with the Server task, when the rendezvous occurs, and how the Server exits after inactivity.

![RendezVous_Sequence_Diagram](./diagrams/rendezvous_sequence_diagram.png)

### Comparison with Java

In Java, communication and synchronization between threads is typically done using:

- synchronized methods or blocks,
- shared variables with manual locking (ReentrantLock, volatile),
- low-level primitives like wait() and notify().

While powerful, these mechanisms are error-prone and require the programmer to manually ensure synchronization and mutual exclusion.

In contrast, Ada provides a built-in, safer mechanism: the rendezvous.

A rendezvous in Ada:

- synchronizes the caller and the callee automatically,
- ensures mutual exclusion (only one task at a time is accepted),
- allows structured data exchange via entry parameters,
- makes concurrency explicit and verifiable by the compiler.

#### Ada (safe, explicit rendezvous)

```ada
-- Client task
Server.Print_Message("Hello!");

-- Server task
accept Print_Message(Msg : String) do
    Put_Line(Msg);
end;
```

#### Java (manual synchronization, less safe)

```java
class Server {
    public synchronized void printMessage(String msg) {
        System.out.println(msg);
    }
}

// In a thread
server.printMessage("Hello");
```

In Java:

- synchronized ensures mutual exclusion inside the method,
- but does not synchronize the client and server — the caller continues immediately,
- there’s no rendezvous, just a protected method.

To achieve similar synchronization, Java would require more complex constructs using wait() and notify(), which are harder to reason about and easier to misuse.

In summary:

- Ada forces you to structure concurrent communication safely and clearly.
- Java gives you flexibility, but puts the burden of correctness on the programmer.

### Useful Links

<https://learn.adacore.com/courses/Ada_For_The_CPP_Java_Developer/chapters/11_Concurrency.html>

## 5. Protected Objects and Types

In Ada, protected objects are used to manage shared data between multiple tasks safely and efficiently. They are similar to monitors or synchronized objects in other languages, but are built into the language and enforced by the compiler.

Protected objects combine:

- Mutual exclusion: Only one task can execute a protected operation at a time.
- Data encapsulation: State is stored inside the object and cannot be accessed directly.
- Optional synchronization: Using entry and when conditions.

### Protected Types vs Protected Objects

- A protected type defines the interface and internal data (like a class).
- A protected object is an instance of a protected type (like an object).

### Example

A complete example of a protected type with its object is provided in:

```bash
src/focus_points/protected_objects_types.adb
```

This program defines a Safe_Counter protected type with:

- a procedure Increment to increase the internal counter,
- a function Get to retrieve the current value.

Two tasks (Worker_1 and Worker_2) increment the counter concurrently. The protected object ensures that access to the counter is safe and mutually exclusive.

Even though two tasks operate in parallel, the counter is incremented correctly, without data races.

### Comparison with Java

In Java, managing shared data between threads typically involves using:

- synchronized methods or blocks,
- explicit locks (ReentrantLock),
- or low-level primitives like wait() and notify().

To simplify concurrent access to numeric variables, Java also provides atomic classes such as AtomicInteger and AtomicBoolean, which allow safe updates without locks — but only for very specific use cases.

```java
class Counter {
    private int count = 0;

    public synchronized void increment() {
        count++;
    }

    public synchronized int get() {
        return count;
    }
}
```

This ensures mutual exclusion, but puts the responsibility for correctness on the programmer. There’s no compiler help for complex coordination.

```java
import java.util.concurrent.atomic.AtomicInteger;

AtomicInteger counter = new AtomicInteger(0);

counter.incrementAndGet();  // atomic increment
int value = counter.get();  // safe read
```

This is more efficient than synchronized, but only supports simple operations. You cannot define complex, condition-based logic like in Ada’s entry ... when.

In contrast, Ada provides protected objects:

- Defined explicitly via protected type,
- Enforced by the compiler,
- Support atomic procedures, safe functions, and conditional entries.

## Reflection

## Davide Ceresa

Working with Ada has been a formative experience. At first, I struggled with its strict syntax and formality, especially compared to more permissive languages like Java or Python. Even working with basic structures such as strings was not always intuitive — I often had to use Unbounded_String to make things simpler and more flexible.

One of the most rewarding aspects of Ada was its built-in approach to concurrency. Once I understood the task model, the rendezvous mechanism, and protected objects, I realized how powerful and well-integrated these features are. Compared to Java, where concurrency often requires additional libraries and boilerplate code, Ada provides a clear, structured model for concurrent programming directly out of the box.

That said, the learning curve was real. Ada is not widely used today, and finding resources, examples, or community support was sometimes difficult. Documentation is available, but not always easy to digest without prior exposure to the language’s mindset.

In the end, although I don’t think I will use Ada again in future personal or professional projects, I understand why it is used in safety-critical domains. Its language design enforces correctness, and its concurrency model encourages writing reliable and predictable code. For applications where failure is not an option, Ada’s philosophy makes perfect sense.

## Dan Livingston

Before the PCP module, I had never even heard of Ada. So when we chose it as the language for our assignment, it was a bit of a shot in the dark. We knew the module was about exploring different programming paradigms, and Ada seemed interesting because of its focus on safety and structure. It definitely turned out to be a challenge — but one that taught a few things.

Ada is extremely strict. Everything from the typing system to its compile-time checks and features like design by contract made us think carefully about how we wrote and structured our code. It’s clearly built for reliability, and I can see why it's used in industries like aerospace and defence. It forces a kind of discipline that you don’t get with more relaxed languages like Python or JavaScript that I'm used to.

That said, actually working with Ada wasn’t always smooth. Debugging, in particular, was frustrating. IDE support was pretty weak, we couldn’t figure out a way to run single files easily, which made testing small pieces of code awkward. Setting breakpoints didn’t work properly. Because of that, we ended up making a little interactive program that could run our demos in a more controlled way, just to make development easier.

To make things worse, different Ada compilers behaved slightly differently, especially around contract-based features, which meant something that worked in one environment could randomly break in another. And since Ada isn’t widely used anymore, finding help online was tough. Stack Overflow had limited answers, and a lot of the places where Ada is used don’t exactly have open or active developer communities. It felt like we were pretty much on our own most of the time.

Still, choosing Ada was a good learning experience. It pushed me to think more carefully about correctness, structure, and program design. Even though I probably won’t be using Ada in the future, it gave me a better appreciation for programming languages that prioritize reliability and safety. It also reminded me of the challenges that come with working in niche or legacy systems.
