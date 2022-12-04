import { json2yaml } from "json2yaml/mod.ts";
const systems = ['x86_64-darwin', 'x86_64-linux'] as const;

type System = typeof systems[number];


type Collection = Record<string, { type: string, [k: string]: string }>;
type SystemCollection = Record<System, Collection>;

type FlakeOutput = {
  apps: SystemCollection,
  nixosConfigurations: Collection,
  nixosModules: Collection,
  packages: SystemCollection,
  [key: string]: Collection | SystemCollection | string,
};

function includes<T extends U, U>(coll: ReadonlyArray<T>, el: U): el is T {
  return coll.includes(el as T);
}

const unwrapSystemCollection = (collection: SystemCollection | Collection): Collection => {
  const keys = Object.keys(collection);
  if (keys.every((key) => includes(systems, key)) && systems.every((system) => keys.includes(system))) {
    return collection[systems[0]] as Collection;
  }
  return collection as Collection;
}



type Task = { cmds: Array<string> };

type TaskFile = {
  version: string,
  tasks: Record<string, Task>,
};

const getNixosFlakePath = (hostName: string) => `.#nixosConfigurations.${hostName}`;
const getPackageFlakePath = (packageName: string) => `.#${packageName}`;

const getNixosFlakeBuildPath = (hostName: string) => `${getNixosFlakePath(hostName)}.config.system.build.toplevel`;

const getBuildCmd = (
  name: string,
  buildPathFunc: (name: string) => string
) => `nix build ${buildPathFunc(name)}`;

const wrapCmdInTask = (cmd: string): Task => ({ cmds: [cmd] });

const getTasks = (prefix: string, buildPathFunc: (name: string) => string, collection: Collection | SystemCollection) =>
  Object.keys(unwrapSystemCollection(collection))
    .reduce((prev, curr) => ({  ...prev, [`${prefix}-${curr}`]: wrapCmdInTask(getBuildCmd(curr, buildPathFunc)) }), {});

const transformFlakeOutput = (flake: FlakeOutput): TaskFile => ({
  version: '3',
  tasks: {
    ...getTasks('build-nixos', getNixosFlakeBuildPath, flake.nixosConfigurations),
    ...getTasks('build-package', getPackageFlakePath, flake.packages),
  },
});


const p = Deno.run({
  cmd: ['nix', 'flake', 'show', '--json'],
  stdout: 'piped',
  stderr: 'piped',
});

const [status, stdout, stderr] = await Promise.all([
  p.status(),
  p.output(),
  p.stderrOutput(),
]);

if (!status.success) {
  console.error(stderr);
  Deno.exit(status.code);
}


const flakeOutput: FlakeOutput = JSON.parse(new TextDecoder().decode(stdout));
const yaml = json2yaml(JSON.stringify(transformFlakeOutput(flakeOutput)));

await Deno.stdout.write(new TextEncoder().encode(yaml));
