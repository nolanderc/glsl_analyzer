
set dotenv-load

target := env_var_or_default("TARGET", "native")
optimize := "Debug"

flags := "-freference-trace=10 -Dtarget="+target+" -Doptimize="+optimize

run:
    zig build run --summary none {{flags}}

watch command="build":
    watchexec -e zig,vert,frag,comp -c -- 'just {{command}} && echo ok'

build:
    zig build --summary none {{flags}}

test:
    zig build test --summary all {{flags}}

clean:
    rm -rf zig-cache zig-out
