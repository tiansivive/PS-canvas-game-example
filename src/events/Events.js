"use strict"




exports.onKeyDown = mEffect => key => handler => _ => {
    window.addEventListener("keydown", e => {
        if (e.code === key) {
            handler()
        }
    })
}