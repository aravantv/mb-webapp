import { Ref, extend } from './lib'
import { ContainerAModel } from './abstractModel'
import { WithKey } from './editors/common'
import { MMElement } from './wm3'

// Abstraction where the concrete model and the abstract one are of the same list type.
export function trivialListAbstraction<P, M extends MMElement<any>>(m: Ref<M[]>, factory: (v: P) => M): ContainerAModel<P, M> {
    return {
        get: () => {
            return m.content.map((v: M): M & WithKey => extend(v, { key: v.uuid }))
        },
        add: (v: P) => {
            m.content.unshift(factory(v))
        },
        addAt: (idx: number, v: P) => { m.content.splice(idx, 0, factory(v)) },
        removeAt: (idx: number) => { m.content.splice(idx, 1) },
    }
}
