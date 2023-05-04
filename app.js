const data = {
  'a': 0,
  'b': [[1, 2], [3, 4]],
  'c': 5
};

function hello (first, second) {
  if (first && second) {
    throw new Error('no');
  } else if (first) {
    console.log('hello');
  } else if (second) {
    console.log('world');
  }
  return 'done';
}

// Exemplo de uso
try {
  hello(false, true);
} catch (error) {
  console.error(error.message);
}

