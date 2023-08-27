# bench/generate.sh
echo "Using params $*"
RUNTIME=$(command -v podman || command -v docker)
$RUNTIME run --mount type=bind,source="$(git rev-parse --show-toplevel)",target=/data,z -w /opt/storm/build/bin -it --rm --name storm docker.io/movesrwth/storm:1.8.0 bash -c "apt install hyperfine time && /data/bench/storm.sh $*"
