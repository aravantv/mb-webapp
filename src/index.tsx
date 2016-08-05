/* tslint:disable */
import * as React from 'react'
/* tslint:enable */
import DevTools from 'mobx-react-devtools'
import { render } from 'react-dom'
import { Class, ClassList } from './metamodel'
import { theClasses, createClass } from './model'
import { WithKey } from './editors/common'
import { selectableText } from './editors/selectableText'
import { listWithUniqueSelection } from './editors/listWithUniqueSelection'
import { trivialAbstraction } from './abstractModel'
import { trivialListAbstraction } from './wm3AbstractModel'

const editor = listWithUniqueSelection<ClassList, Class & WithKey>(
    (cs: ClassList) => trivialListAbstraction(cs.content.classes, createClass),
    selectableText((m: Class) => trivialAbstraction(m.content.name)))

const Comp = editor(theClasses).ReactComponentClass

render(
    <div><Comp/><DevTools/></div>,
    document.getElementById('app')
)
