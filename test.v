dto MyDto {
    int a, // this is a comment
    real b,
    map c,
    str d,
    MyDto next,
}

pub fn foo(int a, str b): int {
    
}

trait MyTrait {
   fn bar(): int; 
   fn baz(int i): int {

   }
}

int i = 1 + 2 * 3 + 4 - 5;

int a = b = c = d;

fn foo (): bool {
    while a mod 2 < 100 {
        return true;
    }
}

a;

fn baz(MyDto options): int {
    for i in 10 {
        continue;
    }

    while val == true  {
        break;
    }
}

3.14159;

(1 + (2 - 1));

class Connection {
    pub int pool_size = 10;

    bool is_initialized;

    pub fn new(int size): self {
        return self;        
    }

    fn init(): bool {
        is_initialized = true;
        return true;
    }
}

bool v = a.b.foo().c;

int i = foo(1, 2, a, b, id, );
foo();
Connection conn = Connection(5);
conn.init();

true + 1;

"foo" + "bar";

bool b = 1 < 2 < 3 + 4 < 5;

@println("testing");

@exit(1 + 2);
