exports.unsafeArraySetAt = <A>(i: number) => (v: A) => (arr: Array<A>) => {
  arr[i] = v;
  return arr;
};
