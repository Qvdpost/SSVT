describe('loop', () => {
  it('should result in 15 for n=5 and a sum function', () => {
    let result = 0;
    loop(5, n => result += n);
    expect(result).toBe(15);
  });
});
