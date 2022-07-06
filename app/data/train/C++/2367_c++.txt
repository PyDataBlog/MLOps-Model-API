//
// Created by chan on 2017/9/19.
//

#include "AndroidRenderer.h"
#include "../misc/RawString2JavaStringHolder.h"

void AndroidRenderer::begin() {
    mJNIEnv->CallVoidMethod(mJavaRenderer, mBeginId);
}

void AndroidRenderer::end() {
    mJNIEnv->CallVoidMethod(mJavaRenderer, mEndId);
}

void AndroidRenderer::renderTitle(RENDERER_UNIT unit, const Text &content) {
    jint titleSize = mTitleSize5;
    switch (unit) {
        case RENDERER_UNIT::TITLE_1:
            titleSize = mTitleSize1;
            break;
        case RENDERER_UNIT::TITLE_2:
            titleSize = mTitleSize2;
            break;
        case RENDERER_UNIT::TITLE_3:
            titleSize = mTitleSize3;
            break;
        case RENDERER_UNIT::TITLE_4:
            titleSize = mTitleSize4;
            break;
        default:
            break;
    }
    RawString2JavaStringHolder holder(mJNIEnv, content);
    jstring jContent = holder.toJstring();
    mJNIEnv->CallVoidMethod(mJavaRenderer, mRenderTitleId, titleSize, jContent);
}

void AndroidRenderer::renderTexture(const Text &content) {
    RawString2JavaStringHolder holder(mJNIEnv, content);
    mJNIEnv->CallVoidMethod(mJavaRenderer, mRenderTextureId, holder.toJstring());
}

void AndroidRenderer::renderTypeface(RENDERER_UNIT unit, const Text &content) {
    jint typeface = mTypefaceItalic;
    if (unit == RENDERER_UNIT::BOLD) {
        typeface = mTypefaceBold;
    }
    RawString2JavaStringHolder holder(mJNIEnv, content);
    mJNIEnv->CallVoidMethod(mJavaRenderer, mRenderTypefaceId, typeface, holder.toJstring());
}

void AndroidRenderer::renderOrderedList(const Text &num, const Text &content) {
    RawString2JavaStringHolder numHolder(mJNIEnv, num);
    RawString2JavaStringHolder contentHolder(mJNIEnv, content);
    mJNIEnv->CallVoidMethod(mJavaRenderer, mRenderOrderedListId, numHolder.toJstring(),
                            contentHolder.toJstring());
}

void AndroidRenderer::renderUnorderedList(const Text &content) {
    RawString2JavaStringHolder holder(mJNIEnv, content);
    mJNIEnv->CallVoidMethod(mJavaRenderer, mRenderUnorderedListId, holder.toJstring());
}

void AndroidRenderer::renderNewLine() {
    mJNIEnv->CallVoidMethod(mJavaRenderer, mRenderNewLineId);
}

void AndroidRenderer::renderImage(const Text &label, const Text &url) {
    RawString2JavaStringHolder labelHolder(mJNIEnv, label);
    RawString2JavaStringHolder urlHolder(mJNIEnv, url);
    mJNIEnv->CallVoidMethod(mJavaRenderer, mRenderImageId, labelHolder.toJstring(),
                            urlHolder.toJstring());
}

void AndroidRenderer::renderLink(const Text &label, const Text &url) {
    RawString2JavaStringHolder labelHolder(mJNIEnv, label);
    RawString2JavaStringHolder urlHolder(mJNIEnv, url);
    mJNIEnv->CallVoidMethod(mJavaRenderer, mRenderLinkId, labelHolder.toJstring(),
                            urlHolder.toJstring());
}

void AndroidRenderer::renderReference(const Text &content, bool append) {
    RawString2JavaStringHolder holder(mJNIEnv, content);
    mJNIEnv->CallVoidMethod(mJavaRenderer, mRenderReferenceId, holder.toJstring(), append);
}

AndroidRenderer::AndroidRenderer(JNIEnv *jNIEnv, jobject &javaRenderer) : mJNIEnv(jNIEnv),
                                                                          mJavaRenderer(
                                                                                  javaRenderer) {
    mJavaClass = mJNIEnv->FindClass("com/chan/mulan/renderer/MarkdownRenderer");
    mBeginId = mJNIEnv->GetMethodID(mJavaClass, "begin", "()V");
    mEndId = mJNIEnv->GetMethodID(mJavaClass, "end", "()V");
    mRenderTitleId = mJNIEnv->GetMethodID(mJavaClass, "renderTitle", "(ILjava/lang/String;)V");
    mRenderTextureId = mJNIEnv->GetMethodID(mJavaClass, "renderTexture", "(Ljava/lang/String;)V");
    mRenderTypefaceId = mJNIEnv->GetMethodID(mJavaClass, "renderTypeface", "(ILjava/lang/String;)V");
    mRenderOrderedListId = mJNIEnv->GetMethodID(mJavaClass, "renderOrderedList", "(Ljava/lang/String;Ljava/lang/String;)V");
    mRenderUnorderedListId = mJNIEnv->GetMethodID(mJavaClass, "renderUnorderedList", "(Ljava/lang/String;)V");
    mRenderNewLineId = mJNIEnv->GetMethodID(mJavaClass, "renderNewLine", "()V");
    mRenderImageId = mJNIEnv->GetMethodID(mJavaClass, "renderImage", "(Ljava/lang/String;Ljava/lang/String;)V");
    mRenderLinkId = mJNIEnv->GetMethodID(mJavaClass, "renderLink", "(Ljava/lang/String;Ljava/lang/String;)V");
    mRenderReferenceId = mJNIEnv->GetMethodID(mJavaClass, "renderReference", "(Ljava/lang/String;Z)V");

    jfieldID field = mJNIEnv->GetStaticFieldID(mJavaClass, "TITLE_SIZE_1", "I");
    mTitleSize1 = mJNIEnv->GetStaticIntField(mJavaClass, field);

    field = mJNIEnv->GetStaticFieldID(mJavaClass, "TITLE_SIZE_2", "I");
    mTitleSize2 = mJNIEnv->GetStaticIntField(mJavaClass, field);

    field = mJNIEnv->GetStaticFieldID(mJavaClass, "TITLE_SIZE_3", "I");
    mTitleSize3 = mJNIEnv->GetStaticIntField(mJavaClass, field);

    field = mJNIEnv->GetStaticFieldID(mJavaClass, "TITLE_SIZE_4", "I");
    mTitleSize4 = mJNIEnv->GetStaticIntField(mJavaClass, field);

    field = mJNIEnv->GetStaticFieldID(mJavaClass, "TITLE_SIZE_5", "I");
    mTitleSize5 = mJNIEnv->GetStaticIntField(mJavaClass, field);

    field = mJNIEnv->GetStaticFieldID(mJavaClass, "TYPEFACE_BOLD", "I");
    mTypefaceBold = mJNIEnv->GetStaticIntField(mJavaClass, field);

    field = mJNIEnv->GetStaticFieldID(mJavaClass, "TYPEFACE_ITALIC", "I");
    mTypefaceItalic = mJNIEnv->GetStaticIntField(mJavaClass, field);
}
