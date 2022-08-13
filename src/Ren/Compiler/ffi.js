export const FFI = global.XMLHttpRequest = class FFI {
    static modules = {}
    static addModule(name, methods) {
        FFI.modules[name] = methods
    }

    listeners = {}

    addEventListener(type, listener) {
        this.listeners[type] = listener
    }

    open(method, url) {
        const [module, fn] = url.split(':')

        this.method = method
        this.module = module
        this.fn = fn
    }

    async send(params) {
        try {
            const fn = FFI.modules[this.module][this.fn]
            const args = JSON.parse(params)

            this.status = 200
            this.statusText = 'ok'

            this.responseURL = `${this.module}:${this.fn}`

            this.response = await args === null ? fn : fn(...args)
            this.response = await this.response

            this.response = JSON.stringify(this.response ?? null)

            this.listeners.load()
        } catch (e) {
            this.status = 400
            this.statusText = e.toString()

            this.response = [`[Error calling ${this.module}:${this.fn}]`, e].join('\n')

            this.listeners.error()
        }
    }

    setRequestHeader() { /* This is stubbed out because Elm calls this method */ }
    getAllResponseHeaders() { /* This is stubbed out because Elm calls this method */ }
}