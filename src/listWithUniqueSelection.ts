import { Stream } from 'xstream'
import { li, ul, input, VNode } from '@cycle/dom'
import { DOMSource } from '@cycle/dom/xstream-typings'
import { isEnter, getText, extend } from './lib'
import { SelectableText, keyMousePreprocessor, Input as ItemInput } from './selectableText'
import Collection from '@cycle/collection'

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

export function ListWithUniqueSelection(ri: RawInput): Output {
  const keyups$ = ri.dom.select('.field2').events('keyup')
  const acceptNew$ = keyups$.filter(isEnter).map(getText)
  const add$: Stream<ItemInput> =
    acceptNew$
      .map(v => {
        const subInput = keyMousePreprocessor(ri)
        return extend({ initValidatedValue: v, initIsSelected: false}, subInput)
      })
  const list$ = Collection(SelectableText, {}, add$).debug()

  let itemVtrees$ = Collection.pluck(list$, (item: Output) => item.dom)
  const field = input('.field2', { props: { type: 'text', value: get_hack_last_value() } })
  const dom = itemVtrees$.map(([...addedComps]) => ul([field, ...addedComps].map(item => li([item]))))
  return {
    dom,
  }
}
