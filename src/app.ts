import { ListWithUniqueSelection } from './listWithUniqueSelection'
import { SelectableText } from './selectableText'
import { makeDOMDriver } from '@cycle/dom'
import { run } from '@cycle/xstream-run'

run(ListWithUniqueSelection, {
  dom: makeDOMDriver('#app'),
})
