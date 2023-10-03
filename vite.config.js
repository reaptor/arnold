export default {
    clearScreen: false,
    root: 'src/Client',
    server: {
        watch: {
            ignored: [
                "**/*.fs" // Don't watch F# files
            ]
        }
    }
}