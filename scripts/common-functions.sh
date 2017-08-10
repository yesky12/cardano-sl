#!/usr/bin/env bash

base_common="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

function find_binary {
  pushd $base_common/.. > /dev/null
  binpath=$(stack path --local-install-root)/bin
  popd > /dev/null
  echo "$binpath"/$1
}

function find_build_binary {
  pushd $base_common/.. > /dev/null
  binpath=$(stack path --dist-dir)/build
  popd > /dev/null
  echo "$binpath"/$1/$1
}

function ensure_run {
  run_dir="$base_common/../run"
  mkdir -p "$run_dir"
}

LOGS_TIME=`date '+%F_%H%M%S'`

function ensure_logs {
  logs_dir="$base_common/../logs/$LOGS_TIME"
  mkdir -p "$logs_dir"
}

function dump_path {
    ensure_logs
    echo -n "$logs_dir/dump/$1"
}

function logs {
  ensure_logs

  local log_file=$1
  local conf_dir="$logs_dir/conf"
  local template_name="log-templates/log-template.yaml"
  if [[ "$LOG_TEMPLATE" != "" ]]; then
    template_name="$LOG_TEMPLATE"
  fi
  local template="$base_common/$template_name"

  mkdir -p "$conf_dir"
  mkdir -p "$logs_dir/dump"

  local conf_file="$conf_dir/$log_file.yaml"
  cat "$template" \
    | sed "s/{{file}}/$log_file/g" \
    > "$conf_file"
  echo -n " --json-log=$logs_dir/node$i.json "
  echo -n " --logs-prefix $logs_dir --log-config $conf_file "
}

function get_port {
  local i=$1
  local i2=$i
  if [[ $i -lt 10 ]]; then
    i2="0$i"
  fi
  echo "30$i2"
}

function dht_key {
  local i=$1
  local i2=$i
  if [[ $i -lt 10 ]]; then
    i2="0$i"
  fi

  $(find_binary cardano-dht-keygen) -n 000000000000$i2 | tr -d '\n'
}

function node_cmd {
  local i=$1
  local is_stat=$2
  local stake_distr=$3
  local wallet_args=$4
  local system_start=$5
  local config_dir=$6
  local st=''
  local reb=''
  local no_ntp=''
  local ssc_algo=''
  local web=''

  ensure_run

  keys_args="--vss-genesis $i --spending-genesis $i"
  if [[ "$CSL_PRODUCTION" != "" ]]; then
      keys_args="--keyfile \"secrets/secret-$((i+1)).key\""
  fi

  if [[ "$SSC_ALGO" != "" ]]; then
    ssc_algo=" --ssc-algo $SSC_ALGO "
  fi
  if [[ $NO_REBUILD == "" ]]; then
    reb=" --rebuild-db "
  fi
  if [[ $NO_NTP != "" ]]; then
    no_ntp=" --no-ntp "
  fi
  if [[ $is_stat != "" ]]; then
    stats=" --stats "
  fi
  if [[ "$REPORT_SERVER" != "" ]]; then
    report_server=" --report-server $REPORT_SERVER "
  fi
  if [[ $i == "0" ]]; then
    if [[ $CARDANO_WEB != "" ]]; then
      web=" --web "
    fi
  fi
  if [[ "$CSL_RTS" != "" ]] && [[ $i -eq 0 ]]; then
    rts_opts="+RTS -N -pa -A6G -qg -RTS"
  fi

  echo -n "$(find_binary cardano-node) --db-path $run_dir/node-db$i $rts_opts $reb $no_ntp $keys_args"

  ekg_server="127.0.0.1:"$((8000+$i))
  statsd_server="127.0.0.1:"$((8125+$i))

  echo -n " --address 127.0.0.1:"`get_port $i`
  echo -n " --listen 127.0.0.1:"`get_port $i`
  echo -n " $(logs node$i.log) $time_lord $stats"
  echo -n " $stake_distr $ssc_algo "
  echo -n " $web "
  echo -n " $report_server "
  echo -n " $wallet_args "
  echo -n " --system-start $system_start"
  echo -n " --metrics +RTS -T -RTS"
  echo -n " --ekg-server $ekg_server"
  #echo -n " --statsd-server $statsd_server"
  echo -n " --node-id node$i"
  echo -n " --topology $config_dir/topology$i.yaml"
  echo -n " --kademlia $config_dir/kademlia$i.yaml"
  echo ''
  sleep 0.8
}

function bench_cmd {
  local i=$1
  local stake_distr=$2
  local system_start=$3
  local time=$4
  local conc=$5
  local delay=$6
  local sendmode=$7
  ensure_run

  echo -n "$(find_binary cardano-wallet)"
  for j in $((i-1))
  do
      echo -n " --peer 127.0.0.1:"`get_port $j`
  done
  echo -n " $(logs node_lightwallet.log)"
  echo -n " --system-start $system_start"
  echo -n " $stake_distr"
  echo -n " cmd --commands \"send-to-all-genesis $time $conc $delay $sendmode tps-sent.csv\""

  echo ''
}



function has_nix {
    which nix-shell 2> /dev/null
    return $?
}

function stack_build {
    if [[ `has_nix` == 0 ]]; then
        echo "Building with nix-shell"
        stack --nix build --test --no-run-tests --bench --no-run-benchmarks --fast
    else
        echo "Building normally"
        stack build --test --no-run-tests --bench --no-run-benchmarks --fast
    fi
}
