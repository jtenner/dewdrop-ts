import * as fsp from "node:fs/promises";

export async function* chars_from(path: string) {
  const f = await fsp.open(path);
  const e = new TextDecoder("utf-8");
  const maxSize = 64 * 1024;
  const buffer = new Uint8Array(maxSize);
  let offset = 0;
  const size = (await f.stat()).size;

  while (true) {
    const { bytesRead } = await f.read({
      buffer,
      length: Math.min(maxSize, size - offset),
      offset,
    });
    if (bytesRead === 0) return;
    const decoded = e.decode(buffer.subarray(0, bytesRead), {
      stream: true,
    });
    for (const c of decoded) {
      yield c;
    }
    offset += bytesRead;
  }
}

export async function* chars(text: string) {
  for (const c of text) {
    yield c;
  }
}
