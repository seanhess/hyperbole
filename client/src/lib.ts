

export function takeWhileMap<T, A>(pred: (val: T) => A | undefined, lines: T[]): A[] {
  var output = []
  for (var line of lines) {
    let a = pred(line)
    if (a)
      output.push(a)
    else
      break;
  }

  return output
}

export function dropWhile<T, A>(pred: (val: T) => A | undefined, lines: T[]): T[] {
  let index = 0;
  while (index < lines.length && pred(lines[index])) {
    index++;
  }
  return lines.slice(index);
}

