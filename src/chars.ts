import * as fsp from "node:fs/promises";

export async function* chars_from(path: string) {
  const f = await fsp.open(path);
  const e = new TextDecoder("utf-8");
  const size = 64 * 1024;
  const buffer = new Uint8Array(size);
  let offset = 0;

  while (true) {
    const { bytesRead } = await f.read({ buffer, length: size, offset });
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
