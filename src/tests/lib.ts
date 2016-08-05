jest.unmock('../lib')

import { none, some, extend } from '../lib'

describe('Option', () => {
    it('none is none', () => {
        expect(none.isNone()).toEqual(true)
    })
    it('some(1) value ', () => {
        expect(some(1).getValue()).toEqual(1)
    })
    it('some(1).is(1) ', () => {
        expect(some(1).is(1)).toEqual(true)
    })
})

describe('extend', () => {
    it('extend({},{})', () => {
        expect(extend({}, {})).toEqual({})
    })
    it('extend({a: 1},{})', () => {
        expect(extend({ a: 1 }, {})).toEqual({ a: 1 })
    })
    it('extend({},{a: 1})', () => {
        expect(extend({}, { a: 1 })).toEqual({ a: 1 })
    })
    it('extend({a:1},{b: true})', () => {
        expect(extend({ a: 1 }, { b: true })).toEqual({ a: 1, b: true })
    })
})