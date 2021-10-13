import { expect } from 'chai';

import { uvaMutator as sut } from '../../../src/mutators/uva-mutator';
import { expectJSMutation } from '../../helpers/expect-mutation';

describe(sut.name, () => {
  it('should have name "uvaMutator"', () => {
    expect(sut.name).eq('uvaMutator');
  });

  it('should mutate += and -=', () => {
    expectJSMutation(sut, 'a += b', 'a -= b');
    expectJSMutation(sut, 'a -= b', 'a += b');
  });

  it('should mutate *=, %= and /=', () => {
    expectJSMutation(sut, 'a *= b', 'a /= b');
    expectJSMutation(sut, 'a /= b', 'a *= b');
    expectJSMutation(sut, 'a %= b', 'a *= b');
  });

  it('should mutate *=, %= and /=', () => {
    expectJSMutation(sut, 'a *= b', 'a /= b');
    expectJSMutation(sut, 'a /= b', 'a *= b');
    expectJSMutation(sut, 'a %= b', 'a *= b');
  });

  it('should mutate <<=, >>=, &= and |=', () => {
    expectJSMutation(sut, 'a *= b', 'a /= b');
    expectJSMutation(sut, 'a /= b', 'a *= b');
    expectJSMutation(sut, 'a %= b', 'a *= b');
  });

  it('should mutate &&=, ||= and ??=', () => {
    expectJSMutation(sut, 'a &&= b', 'a ||= b');
    expectJSMutation(sut, 'a ||= b', 'a &&= b');
    expectJSMutation(sut, 'a ??= b', 'a &&= b');
  });

  it('should not mutate a string literal unless it is &&=, ||=, ??=', () => {
    expectJSMutation(sut, 'a += "b"', 'a += "b"');
    expectJSMutation(sut, 'a -= "b"', 'a -= "b"');
    expectJSMutation(sut, 'a *= "b"', 'a *= "b"');
    expectJSMutation(sut, 'a /= "b"', 'a /= "b"');
    expectJSMutation(sut, 'a %= "b"', 'a %= "b"');
    expectJSMutation(sut, 'a <<= "b"', 'a <<= "b"');
    expectJSMutation(sut, 'a >>= "b"', 'a >>= "b"');
    expectJSMutation(sut, 'a &= "b"', 'a &= "b"');
    expectJSMutation(sut, 'a |= "b"', 'a |= "b"');
  });

  it('should mutate a string literal using &&=, ||=, ??=', () => {
    expectJSMutation(sut, 'a &&= "b"', 'a ||= "b"');
    expectJSMutation(sut, 'a ||= "b"', 'a &&= "b"');
    expectJSMutation(sut, 'a ??= "b"', 'a &&= "b"');
  });

  it('should not mutate string template unless it is &&=, ||=, ??=', () => {
    expectJSMutation(sut, 'a += `b`', 'a += `b`');
    expectJSMutation(sut, 'a -= `b`', 'a -= `b`');
    expectJSMutation(sut, 'a *= `b`', 'a *= `b`');
    expectJSMutation(sut, 'a /= `b`', 'a /= `b`');
    expectJSMutation(sut, 'a %= `b`', 'a %= `b`');
    expectJSMutation(sut, 'a <<= `b`', 'a <<= `b`');
    expectJSMutation(sut, 'a >>= `b`', 'a >>= `b`');
    expectJSMutation(sut, 'a &= `b`', 'a &= `b`');
    expectJSMutation(sut, 'a |= `b`', 'a |= `b`');
  });

  it('should mutate string template using &&=, ||=, ??=', () => {
    expectJSMutation(sut, 'a &&= `b`', 'a ||= `b`');
    expectJSMutation(sut, 'a ||= `b`', 'a &&= `b`');
    expectJSMutation(sut, 'a ??= `b`', 'a &&= `b`');
  });
});
