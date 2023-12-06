
set dotenv-load
set positional-arguments

target := env_var_or_default("TARGET", "native")
optimize := "Debug"

flags := "-freference-trace=10 -Dtarget="+target+" -Doptimize="+optimize

build *ARGS:
    zig build --summary none {{flags}} "$@"

run *ARGS:
    zig build run --summary none {{flags}} -- {{ARGS}}

watch *ARGS:
    #!/usr/bin/bash
    watchexec -e zig,py,vert,frag,comp -c -- "just ${*@Q} && echo ok"

test:
    zig build test --summary failures {{flags}}
    just install
    pytest tests/

test-file *ARGS:
    zig test {{flags}} "$@"

test-lldb:
    zig build -Dinstall-tests
    lldb --batch --one-line run -- ./zig-out/bin/unit-tests

clean:
    rm -rf zig-cache zig-out

install prefix="$HOME/.local/":
    zig build install --prefix "{{prefix}}" {{flags}}

stderr:
    tail --follow stderr.log

generate-spec:
    cd spec && just

release:
    zig build release {{flags}}
