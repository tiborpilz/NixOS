{ lib, ... }:
with lib;
rec {
  serialize_list = list: concatStrings (lib.intersperse ", " list);
  serialize_value = v:
    if isList v then serialize_list v else
    if isString v then v else
    throw "invalid value type";
  serialize_entry = k: v: "${k} = ${serialize_value v}";
  serialize_set = set: concatstrings intersperce "\n" (mapAttrsToList (k: v: serialize_entry k v) set);
  get_files = folder: mapAttrsToList (k: v: { "${k}.cfg" = v; }) folder;
  get_folders = cfg: mapAttrsToList (k: v: { "${k}" = (get_files v); }) cfg;
}
