import { ref, computed } from 'vue'

function ChildComponent(props: { show: boolean }) {
    const msg = ref('')

    return vine`
      <div class="child-component">
        <p>message: {{ msg }}</p>
        <input v-model="msg" placeholder="Type a message" />
      </div>
    `
}

export function App() {
    const foo = ref('Hello, Vue Vine!')
    const zag = ref(1)
    const bar = computed(() => zag.value + 1)

    return vine`
      <div class="app">
        <h1>{{ foo }}</h1>
        <p>zag: {{ zag }}</p>
        <p>bar: {{ bar }}</p>
        <button @click="zag++">Increment zag</button>

        <ChildComponent !show />
      </div>
    `
}