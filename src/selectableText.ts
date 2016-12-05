import { isEnter, isEscape, getText, extend, Input } from './lib'
import { Stream } from 'xstream'
import { span, input, VNode, li } from '@cycle/dom'
import { DOMSource } from '@cycle/dom/xstream-typings'

export interface RawInput {
  dom: DOMSource
}

export interface Output {
  dom: Stream<VNode>
}

export const enum InputType {
  Confirm, // no payload
  Cancel, // no payload
  Select, // no payload
  Change, // payload: string
}

interface IState {
  uiValue: string,
  validatedName: string,
  isSelected: boolean,
}

function getChange(ev: Event) {
  return { type: InputType.Change, payload: getText(ev) }
}

// "Intent"
export function keyMousePreprocessor(i: RawInput): Input<InputType> {
  const keyups$ = i.dom.select('.field').events('keyup')
  return Stream.merge(
    keyups$.filter(isEnter).mapTo({ type: InputType.Confirm }),
    keyups$.filter(isEscape).mapTo({ type: InputType.Cancel }),
    keyups$.filter(ev => !isEscape(ev) && !isEnter(ev)).map(getChange),
    i.dom.select('.field').events('dblclick').mapTo({ type: InputType.Select })
  )
}

// "Model"
function react$(i: Input<InputType>, initState: IState): Stream<IState> {
  return i.map(n => s => {
    switch (n.type) {
      case InputType.Confirm:
        return extend({ validatedName: s.uiValue, isSelected: false }, s)
      case InputType.Cancel:
        return extend({ uiValue: s.validatedName, isSelected: false }, s)
      case InputType.Change:
        return extend({ uiValue: n.payload }, s)
      case InputType.Select:
        return extend({ isSelected: true }, s)
    }
  }).fold((acc, f: (prev: IState) => IState) => f(acc), initState)
}

// "View"
function render$(s$: Stream<IState>): Stream<VNode> {
  return s$.map(s =>
    s.isSelected
      ? input('.field', { props: { type: 'text', value: s.uiValue } })
      : span('.field', s.validatedName)).map(v => li([v]))
};

export function SelectableText(i: Input<InputType>): Output {
  const state$ = react$(i, { isSelected: true, uiValue: '', validatedName: null })
  return { dom: render$(state$) }
}
