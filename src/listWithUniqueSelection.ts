import { Stream } from 'xstream'
import delay from 'xstream/extra/delay'
import { li, ul, input, VNode } from '@cycle/dom'
import { DOMSource } from '@cycle/dom/xstream-typings'
import { isEnter, getText, extend } from './lib'
import { SelectableText, keyMousePreprocessor } from './selectableText'
import isolate from '@cycle/isolate'

export interface RawInput {
  dom: DOMSource
}

export interface Output {
  dom: Stream<VNode>
}

interface Input {
  added$: Stream<string>,
  deleted$: Stream<number>,
}

type VNodeStream = Stream<VNode>

/** Hack to force the input field to refresh. */
let hack_last_value = ''
function get_hack_last_value() {
  hack_last_value = hack_last_value === '' ? null : ''
  return hack_last_value
}

function ItemComponent(ri: RawInput, initString: string) {
  const subInput = keyMousePreprocessor(ri)
  const change$ = subInput.change$.startWith(initString).debug()
  const confirm$ = subInput.confirm$.startWith(null).compose(delay(1)).debug()
  return SelectableText(extend({ change$, confirm$ }, subInput))
}

export function ListWithUniqueSelection(ri: RawInput): Output {
  const keyups$ = ri.dom.select('.field2').events('keyup')
  const acceptNew$ = keyups$.filter(isEnter).map(getText)
  const addedComponents$: Stream<Array<VNodeStream>> =
    acceptNew$
      .map(v => (state: Array<VNodeStream>): Array<VNodeStream> => {
        const newState = state.slice()
        const newItem = isolate(ItemComponent)(ri, v)
        newState.splice(0, 0, newItem.dom)
        return newState
      })
      .fold((acc, f) => f(acc), [])
  const field = input('.field2', { props: { type: 'text', value: get_hack_last_value() } })
  const dom =
    addedComponents$.map(
      addedCompStreams =>
        Stream.combine(...addedCompStreams)
          .map(([...addedComps]) =>
            ul([field, ...addedComps].map(item => li([item]))))
    ).flatten()
  return {
    dom,
  }
}
