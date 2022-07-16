{ config, lib, pkgs, ... }:
with lib;
let
  port = "8080";
  root_dir = "/var/lib/openhab";




  flattenAttrset = set: prefix: separator:
    let
      flattenOuter = list: foldr (curr: acc: curr // acc) {} list;
      flattenInner = set: foldr (curr: acc: curr // acc) {} set;
      innerMapper = n: v: path: { "${path}${separator}${n}" = v; };
      outerMapper = outer_n: outer_v: flattenInner (mapAttrsToList (n: v: innerMapper n v "${prefix}${outer_n}") outer_v);
    in
      flattenOuter (mapAttrsToList outerMapper set);



  generateConfigScript = cfg:
  let
    serialize_list = list: seperator: concatStrings (intersperse seperator list);

    serialize_value = v:
      if isList v then serialize_list v ", " else
      if isString v then v else
      throw "invalid value type";

    serialize_entry = k: v: "${k} = ${serialize_value v}";
    serialize_set = set: concatStrings (intersperse "\n" (mapAttrsToList (k: v: serialize_entry k v) set));
    get_files = folder: mapAttrsToList (k: v: { name = "${k}.cfg"; content = (serialize_set v); }) folder;
    get_folders = cfg: mapAttrsToList (k: v: { name = "${k}"; files = (get_files v); }) cfg;

    file_script = file: target: ''
      echo '${file.content}' > '${target}/${file.name}'
    '';

    folder_script = folder:
      let target = "${root_dir}/conf/${folder.name}";
      in
      ''
        mkdir -p ${target}
        ${serialize_list (map (file: file_script file target) folder.files) "\n"}
      '';
  in
    ''
      ${serialize_list (map (folder: folder_script folder) (get_folders cfg)) "\n"}
    '';

  openhab_cfg = {
    services = {
      addons = {
        package = "standard";
        binding = ["deconz"];
        ui = ["basic" "habpanel"];
      };
      runtime = {
        "org.openhab.i18n:language" = "en";
      };
    };
  };
in
{
  system.activationScripts.createOpenhabConfig = stringAfter [ "var" ] (generateConfigScript openhab_cfg);
  virtualisation.oci-containers.containers.openhab = {
    ports = [  "${port}:${port}" ];
    image = "openhab/openhab";
    volumes = [
      # "/etc/localtime:/etc/localtime:ro"
      # "/etc/timezone:/etc/timezone:ro"
      "${root_dir}/conf:/openhab/conf"
      "${root_dir}/userdata:/openhab/userdata"
      "${root_dir}/addons:/openhab/addons"
    ];
    environment = {
      "OPENHAB_HTTP_PORT" = "${port}";
    };
  };
}
