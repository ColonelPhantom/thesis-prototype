podman run --mount type=bind,source="$(git rev-parse --show-toplevel)",target=/data,z -w /opt/rubicon -it --rm --name rubicon sjunges/rubicon:cav21 bash -c "/data/bench/rubicon.sh $*"
podman run --mount type=bind,source="$(git rev-parse --show-toplevel)",target=/data,z -w /opt/rubicon -it --rm --name rubicon rubicon-ci bash -c "/data/bench/rubicon-snakes.sh $*"

podman run --mount type=bind,source="$(git rev-parse --show-toplevel)",target=/data,z -it --rm --name dice sholtzen/dice bash -c "/data/bench/dice.sh $*"
