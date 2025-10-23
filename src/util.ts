import * as path from "node:path";

export type FileEntry = {
  absolute: string;
  relative: string;
};

export const to_file_entry = (
  loc: string,
  basedir: string,
  ext: string | null = null,
) => {
  loc = ext && path.extname(loc) !== ext ? `${loc}.ext` : loc;
  return path.isAbsolute(loc)
    ? {
        absolute: loc,
        relative: `./${path.relative(basedir, loc)}`,
      }
    : {
        absolute: path.join(basedir, loc),
        relative: loc,
      };
};

export type Result<TErr, TOk> = { ok: TOk } | { err: TErr };

export type Builtin = {};
