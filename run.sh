rm gorb main
zig build-exe main.zig
mv main gorb
./gorb test.gorb
