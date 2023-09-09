
set dotenv-load

target := env_var_or_default("TARGET", "native")
optimize := "Debug"

flags := "-freference-trace=10 -Dtarget="+target+" -Doptimize="+optimize

run *ARGS:
    zig build run --summary none {{flags}} -- {{ARGS}}

watch command="build":
    watchexec -e zig,py,vert,frag,comp -c -- 'just {{command}} && echo ok'

build:
    zig build --summary none {{flags}}

test:
    zig build test --summary all {{flags}}

clean:
    rm -rf zig-cache zig-out

install prefix="$HOME/.local/":
    zig build install --prefix "{{prefix}}" {{flags}}

stderr:
    tail --follow stderr.log

generate-spec:
    cd spec && just

