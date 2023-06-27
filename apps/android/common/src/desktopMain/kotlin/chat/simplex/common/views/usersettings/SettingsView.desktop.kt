package chat.simplex.common.views.usersettings

import SectionView
import androidx.compose.runtime.Composable
import chat.simplex.common.model.ChatModel
import com.icerockdev.library.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
actual fun SettingsSectionApp(
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showCustomModal: (@Composable (ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  showVersion: () -> Unit,
  withAuth: (title: String, desc: String, block: () -> Unit) -> Unit
) {
  SectionView(stringResource(R.string.settings_section_title_app)) {
    SettingsActionItem(painterResource(R.drawable.ic_code), stringResource(R.string.settings_developer_tools), showSettingsModal { DeveloperView(it, showCustomModal, withAuth) }, extraPadding = true)
    AppVersionItem(showVersion)
  }
}